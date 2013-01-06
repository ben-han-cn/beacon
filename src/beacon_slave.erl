-module(beacon_slave).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {master, services}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1,
         start_service/3,
         stop_service/2,
         run_command/4,
         async_run_command/4,
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

start(Master) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Master], []).

start_service(Slave, Service, Args) ->
    gen_server:call(Slave, {start_service, Service, Args}).

stop_service(Slave, Service) ->
    gen_server:call(Slave, {stop_service, Service}).

get_running_services(Slave) ->
    gen_server:call(Slave, get_running_services).

run_command(Slave, Service, Cmd, Args) ->
    gen_server:call(Slave, {run_cmd, Service, Cmd, Args}).

async_run_command(Slave, Service, Cmd, Args) ->
    gen_server:cast(Slave, {run_cmd, Service, Cmd, Args}).

stop(Slave) ->
    gen_server:cast(Slave, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Master]) ->
    Master ! {slave_started, self()},
    {ok, #state{master = Master, services = []}}.

handle_call({start_service, Service, Args}, _From, #state{services= Services} = State) ->
    spawn(Service, start, [self() | Args]),
    receive
        {service_started, ServicePID} ->
            link(ServicePID),
            {reply, ok, State#state{services = [{Service, ServicePID} | Services]}}
    after 2000 ->
            {reply, failed, State}
    end;

handle_call({run_cmd, Service, Cmd, Args}, _From, State) ->
    apply(Service, Cmd, Args),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

