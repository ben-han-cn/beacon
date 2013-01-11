-module(beacon_port_dirver).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {port}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1,
         sync_run/2,
         async_run/2,
         stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start([Master, Script]) ->
    gen_server:start(?MODULE, [Master, Script], []).

sync_run(Pid, Cmd) ->
    gen_server:call(Pid, {run_cmd, Cmd}). 

async_run(Pid, Cmd) ->
    gen_server:cast(Pid, {run_cmd, Cmd}). 

stop(Pid) ->
    gen_server:cast(Pid, stop).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Master, Script]) ->
    Port = open_port({spawn, Script}, [{packet, 2}]),
    Master ! {service_started, self()},
    {ok, #state{port = Port}}.

handle_call({run_cmd, Cmd}, _From, #state{port = Port} = State) ->
    erlang:port_command(Port, Cmd),
    receive
        {Port, {data, Data}} ->
            {reply, {ok, Data}, State}
    after 2000 ->
            {reply, {failed, timeout}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({run_cmd, Cmd}, #state{port = Port} = State) ->
    erlang:port_command(Port, Cmd),
    {noreply, State};

handle_cast(stop, #state{port = Port} = State) ->
    erlang:port_close(Port),
    {reply, ok, State};

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

