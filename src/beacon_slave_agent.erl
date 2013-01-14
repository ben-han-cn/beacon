-module(beacon_slave_agent).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("beacon.hrl").
-record(state, {node_full_name, slave_pid, slave_state}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         start_service/3,
         stop_service/2,
         run_sync_cmd/4,
         run_async_cmd/4,
         get_running_services/1,
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

start_service(Agent, Service, Args) ->
    gen_server:call(Agent, {start_service, Service, Args}).

stop_service(Agent, Service) ->
    gen_server:call(Agent, {stop_service, Service}).

run_sync_cmd(Agent, Service, Cmd, Args) ->
    gen_server:call(Agent, {run_sync_cmd, Service, Cmd, Args}).

run_async_cmd(Agent, Service, Cmd, Args) ->
    gen_server:cast(Agent, {run_async_cmd, Service, Cmd, Args}).

get_running_services(Agent) ->
    gen_server:call(Agent, get_running_services).

stop(Agent) ->
    gen_server:cast(Agent, stop).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

link_node(Node) ->
    case net_adm:ping(Node) of
        pong -> 
            monitor_node(Node, true),
            true;
        _ ->
            false
    end.

init([Node]) ->
    case link_node(Node) of
        true ->
            spawn(Node, beacon_slave, start, [self()]),
            receive
                {slave_started, SlavePID} ->
                erlang:monitor(process, SlavePID),
                {ok, #state{node_full_name = Node, slave_pid = SlavePID, slave_state = running}} 
            after 
                ?REMOTE_SLAVE_CREATE_TIMEOUT-> {stop, "connect node failed"}
            end;
        false ->
            {stop, "connect node failed"}
    end.

handle_call({start_service, Service, Args}, _From, #state{slave_pid = SlavePID, slave_state = SlaveStatus} = State) ->
    case SlaveStatus of
        running ->
            beacon_slave:start_service(SlavePID, Service, Args);
        _ ->
            io:format("slave node isn't running")
    end,
    {reply, ok, State};

handle_call({run_sync_cmd, Service, Cmd, Args}, _From, #state{slave_pid = SlavePID, slave_state = SlaveStatus} = State) ->
    Result = case SlaveStatus of
                running ->
                    beacon_slave:run_sync_cmd(SlavePID, Service, Cmd, Args);
                _ ->
                    io:format("slave node isn't running"),
                    'cmd is buffered'
            end,
    {reply, Result, State}.

handle_cast({run_async_cmd, Service, Cmd, Args}, #state{slave_pid = SlavePID, slave_state = SlaveStatus} = State) ->
    case SlaveStatus of
        running ->
            beacon_slave:run_async_cmd(SlavePID, Service, Cmd, Args);
        _ ->
            io:format("slave node isn't running")
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', _Reference, process, Pid, _Reason}, #state{slave_pid = SlavePID} = State) ->
    if 
        Pid =:= SlavePID -> 
            io:format("remote slave process down ~n"),
            {noreply, State#state{slave_state = "offline"}};
        true ->
            io:format("unknow pid down ~n"),
            {noreply, State}
    end;

handle_info({nodedown, _FullNode}, #state{node_full_name = _Node} = State) ->
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

