-module(beacon_slave_agent).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {node_full_name, slave_pid, slave_state}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1,
         start_service/3,
         stop_service/2,
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

start(Node) ->
    gen_server:start(?MODULE, [Node], []).

start_service(Agent, Service, Args) ->
    gen_server:call(Agent, {start_service, Service, Args}).

stop_service(Agent, Service) ->
    gen_server:call(Agent, {stop_service, Service}).

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
            after 2000 ->
                {stop, "connect node failed"}
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
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', _Reference, process, Pid, _Reason}, #state{slave_pid = SlavePID} = State) ->
    if 
        Pid =:= SlavePID -> 
            io:fomrat("remote slave process down ~n"),
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

