-module(beacon_echo).
-behaviour(gen_server).
-define(SERVER, echo).
-record(state, {run_cmd_count}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1,
         get_run_cmd_count/1,
         run_cmd/2,
         crash/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Args) ->
    {ok, PID} = gen_server:start({local, ?SERVER}, ?MODULE, Args, []),
    PID.

get_run_cmd_count(Node) ->
    gen_server:call({?SERVER, Node}, get_run_cmd_count).

run_cmd(Node, Cmd) ->
    gen_server:cast({?SERVER, Node}, {run_cmd, Cmd}).

crash(Node) ->
    gen_server:cast({?SERVER, Node}, crash).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Master]) ->
    Master ! {service_started, self()},
    {ok, #state{run_cmd_count = 0}}.

handle_call(get_run_cmd_count, _From, #state{run_cmd_count = Count} = State) ->
    {reply, Count, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({run_cmd, Cmd}, #state{run_cmd_count = Count}) ->
    io:format("run cmd ~p ~n", [Cmd]),
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

