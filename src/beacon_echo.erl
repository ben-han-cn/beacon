-module(beacon_echo).
-behaviour(gen_server).
-define(SERVER, echo).
-record(state, {run_cmd_count}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1,
         get_echo_count/0,
         echo/1,
         crash/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Args) ->
    {ok, PID} = gen_server:start({local, ?SERVER}, ?MODULE, [Args], []),
    PID.

get_echo_count() ->
    gen_server:call(?SERVER, get_echo_count).

echo(Str) ->
    gen_server:cast(?SERVER, {echo, Str}).

crash() ->
    gen_server:cast(?SERVER, crash).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Master]) ->
    Master ! {service_started, self()},
    {ok, #state{run_cmd_count = 0}}.

handle_call(get_echo_count, _From, #state{run_cmd_count = Count} = State) ->
    {reply, Count, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({echo, Str}, #state{run_cmd_count = Count}) ->
    io:format("~p ~n", [Str]),
    {noreply, #state{run_cmd_count = Count + 1}};

handle_cast(crash, State) ->
    io:format("going to die ~p ~n", [self()]),
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

