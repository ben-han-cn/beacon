-module(beacon_slave).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {master, services, cmd_queue}).
-define(SERVICE_MAP, [{ruby, beacon_port_dirver}]).
-include("beacon.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1,
         run_sync_cmd/2,
         run_async_cmd/2,
         run_critical_cmd/2,
         get_last_cmd_id/1,
         stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Master) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Master], []).

run_sync_cmd(Slave, Cmd) ->
    gen_server:call(Slave, {run_sync_cmd, Cmd}).

run_critical_cmd(Slave, Cmd) ->
    gen_server:call(Slave, {run_critical_cmd, Cmd}).

get_last_cmd_id(Slave) ->
    gen_server:call(Slave, get_last_cmd_id).

run_async_cmd(Slave, Cmd) ->
    gen_server:cast(Slave, {run_async_cmd, Cmd}).

stop(Slave) ->
    gen_server:cast(Slave, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Master]) ->
    Master ! {slave_started, self()},
    {ok, #state{master = Master, 
                services = [], 
                cmd_queue = beacon_cmd_queue:start_link(slave, "./slave.cmd_queue")}}. 

handle_call({run_sync_cmd, Cmd}, _From, #state{services = Services} = State) ->
    Service = list_to_atom(beacon_command:get_service(Cmd)),
    case beacon_command:is_start_new_service(Cmd) of 
        true -> start_service(Service, Cmd, State);
        false -> 
            {value, {Service, Module, ServicePid}} = lists:keysearch(Service, 1, Services),
            Result = apply(Module, Cmd, [ServicePid | Cmd#beacon_cmd.args]),
            {reply, Result, State}
    end;

handle_call({run_critical_cmd, Cmd}, _From, #state{services = Services, cmd_queue = Queue} = State) ->
    Service = list_to_atom(beacon_command:get_service(Cmd)),
    beacon_cmd_queue:enqueue(Queue, Cmd),
    case beacon_command:is_start_new_service(Cmd) of 
            true -> start_service(Service, Cmd, State);
            false -> 
                {value, {Service, Module, ServicePid}} = lists:keysearch(Service, 1, Services),
                Result = apply(Module, sync_run, [ServicePid, Cmd]),
                {reply, Result, State}
    end;

handle_call(get_last_cmd_id,  _From, #state{cmd_queue = Queue} = State) ->
    [_, MaxID] = beacon_cmd_queue:get_cmd_id_range(Queue),
    {reply, (MaxID - 1), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({run_async_cmd, Cmd}, #state{services = Services} = State) ->
    Service = list_to_atom(beacon_command:get_service(Cmd)),
    case beacon_command:is_start_new_service(Cmd) of 
        true -> 
            start_service(Service, Cmd, State);
        false ->
            {value, {Service, Module, ServicePid}} = lists:keysearch(Service, 1, Services),
            spawn(Module, Cmd, [ServicePid | Cmd#beacon_cmd.args])
    end,
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({probe, Prober}, State) ->
    Prober ! {ok, self()},
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
start_service(Service, Cmd, #state{services= Services} = State) ->
    {value, {Service, Module}} = lists:keysearch(Service, 1, ?SERVICE_MAP),
    spawn(Module, start, [[self() | Cmd#beacon_cmd.args]]),
    receive
        {service_started, ServicePID} ->
            link(ServicePID),
            {reply, ok, State#state{services = [{Service, Module, ServicePID} | Services]}}
    after 2000 ->
            io:format("!!!!!!! start sevices failed ~n"),
            {reply, failed, State}
    end.
