-module(beacon_master).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {online_nodes,
                offline_nodes}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         add_node/2,
         remove_node/1,
         get_node/1,
         get_nodes/0,
         get_offline_nodes/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_node(NodeName, IP) ->
    gen_server:call(?SERVER, {add_node, NodeName, IP}).


remove_node(NodeName) ->
    gen_server:call(?SERVER, {remove_node, NodeName}).

get_node(NodeName) ->
    gen_server:call(?SERVER, {get_node, NodeName}).

get_nodes() ->
    gen_server:call(?SERVER, get_nodes).

get_offline_nodes() ->
    gen_server:call(?SERVER, get_offline_nodes).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{online_nodes=[], offline_nodes=[]}}.

handle_call({add_node, Node, IP}, _From, #state{online_nodes = OnlineNodes} = State) ->
    NodeFullName= node_full_name(Node, IP),
    case beacon_slave_agent:start_link(NodeFullName) of
        {ok, PID} ->
            {reply, {ok, PID}, State#state{online_nodes = [{Node, PID} | OnlineNodes]}};
        {error, Info} ->
            ErrorInfo = io_lib:format("node ~p add failed : ~p ~n", [Node, Info]),
            {reply, {failed, ErrorInfo}, State}
    end;

handle_call({get_node, Node}, _From, #state{online_nodes = OnlineNodes} = State) ->
    case lists:keysearch(Node, 1, OnlineNodes) of 
        {value, {Node, AgentPID}} ->
            {reply, {ok, AgentPID}, State};
        false ->
            {reply, {failed, undefined}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({'EXIT',Pid, Info}, #state{online_nodes = OnlineNodes} = State) ->
    NewOnlineNodes = case lists:keytake(Pid, 2, OnlineNodes) of
                        {value, {Node, Pid}, LeftOnlineNodes} -> 
                            io:format("node ~p agent crashed ~p ~n", [Node, Info]),
                            LeftOnlineNodes;
                        _ -> 
                            io:format("warning receive unknow process : ~p  exit info", [Pid]),
                            OnlineNodes
                     end,
    {noreply, State#state{online_nodes = NewOnlineNodes}};

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
node_full_name(Node, IP) ->
    list_to_atom(atom_to_list(Node) ++ "@" ++ IP).
