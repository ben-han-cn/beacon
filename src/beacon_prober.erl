-module(beacon_prober).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(PROBE_FREQUENCY, 5000).
-define(PROBE_TIMEOUT, 3000).
-record(state, {master,
                target,
                frequency}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
         start_link/3,
         stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Master, Target) ->
    start_link(Master, Target, ?PROBE_FREQUENCY).
start_link(Master, Target, Frequency) ->
    gen_server:start_link(?MODULE, [Master, Target, Frequency], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Master, Target, Frequency]) ->
    io:format("start to probe ~p ~n", [Target]),
    erlang:send_after(Frequency, self(), proble),
    {ok, #state{master = Master, target = Target, frequency = Frequency}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(proble, State) ->
    #state{target = Target, master = Master, frequency = Frequency} = State,
    Target ! {probe, self()},
    receive
        {ok, Target} -> 
            Master ! {probe_succeed, Target},
            {stop, normal, State}
    after
        ?PROBE_TIMEOUT ->
            erlang:send_after(Frequency, self(), proble),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

