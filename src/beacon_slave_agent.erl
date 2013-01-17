-module(beacon_slave_agent).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("beacon.hrl").
-record(state, {node_full_name, 
                slave_pid, 
                slave_state,
                cmd_queue}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         run_sync_cmd/2,
         run_async_cmd/2,
         run_critical_cmd/2,
         stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Node) ->
    gen_server:start_link(?MODULE, [Node], []).

run_sync_cmd(Agent, Cmd) ->
    gen_server:call(Agent, {run_sync_cmd, Cmd}).

run_critical_cmd(Agent, Cmd) ->
    gen_server:call(Agent, {run_critical_cmd, Cmd}).

run_async_cmd(Agent, Cmd) ->
    gen_server:cast(Agent, {run_async_cmd, Cmd}).

stop(Agent) ->
    gen_server:cast(Agent, stop).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Node]) ->
    case link_node(Node) of
        true ->
            spawn(Node, beacon_slave, start, [self()]),
            receive
                {slave_started, SlavePID} ->
                erlang:monitor(process, SlavePID),
                {ok, #state{node_full_name = Node, 
                            slave_pid = SlavePID, 
                            slave_state = running,
                            cmd_queue = beacon_cmd_queue:start_link(Node, (atom_to_list(Node) ++ ".cmd_queue"))}} 
            after 
                ?REMOTE_SLAVE_CREATE_TIMEOUT-> {stop, "connect node failed"}
            end;
        false ->
            {stop, "connect node failed"}
    end.

handle_call({run_sync_cmd, CmdJson}, _From, #state{slave_pid = SlavePID, slave_state = SlaveStatus} = State) ->
    Result = case SlaveStatus of
                running ->
                    beacon_slave:run_sync_cmd(SlavePID, beacon_command:from_json(CmdJson));
                _ ->
                    {failed, "node is offline"}
            end,
    {reply, Result, State};

handle_call({run_critical_cmd, CmdJson}, _From, #state{slave_pid = SlavePID, slave_state = SlaveStatus, cmd_queue = Queue} = State) ->
    Result = case SlaveStatus of
                running ->
                    Cmd = beacon_cmd_queue:enqueue(Queue, CmdJson),
                    Ret = beacon_slave:run_critical_cmd(SlavePID, Cmd),
                    beacon_cmd_queue:dequeue(Queue),
                    {ok, Ret};
                _ ->
                    beacon_cmd_queue:enqueue(Queue, CmdJson),
                    {ok, "slave node is offline, cmd will be buffered"}
            end,
    {reply, Result, State}.

handle_cast({run_async_cmd, CmdJson}, #state{slave_pid = SlavePID, slave_state = SlaveStatus} = State) ->
    case SlaveStatus of
        running ->
            beacon_slave:run_async_cmd(SlavePID, beacon_command:from_json(CmdJson));
        _ ->
            io:format("warning!   node is offline")
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', _Reference, process, Pid, _Reason}, #state{slave_pid = SlavePid} = State) ->
    if 
        Pid =:= SlavePid -> 
            io:format("remote slave process down ~n"),
            beacon_prober:start_link(self(), Pid),
            {noreply, State#state{slave_state = offline}};
        true ->
            io:format("unknow pid down ~n"),
            {noreply, State}
    end;

handle_info({probe_succeed, Pid}, #state{slave_pid = Pid, slave_state = SlaveState} = State) -> 
    case SlaveState of
        offline -> syncronize_cmd_queue(State)
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
link_node(Node) ->
    case net_adm:ping(Node) of
        pong -> 
            monitor_node(Node, true),
            true;
        _ ->
            false
    end.

syncronize_cmd_queue(State) ->
    #state{slave_pid = SlavePid, cmd_queue = MasterQueue} = State,
    io:format("slave ~p is online again, start to sync command ~n", [SlavePid]),
    SlaveLastCmdID = beacon_slave:get_last_cmd_id(SlavePid),
    beacon_cmd_queue:dequeue_to_cmd(MasterQueue, SlaveLastCmdID),
    Commands = beacon_cmd_queue:to_list(MasterQueue),
    send_buffered_cmds(Commands, SlavePid),
    beacon_cmd_queue:clear(MasterQueue).

send_buffered_cmds([], _) ->
    ok;
send_buffered_cmds([Cmd | Rest], SlavePID) ->
    beacon_slave:run_critical_cmd(SlavePID, Cmd),
    send_buffered_cmds(Rest, SlavePID).
