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
         send_cmd_to_node/2,
         get_run_cmd_count/1,
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

get_nodes() ->
    gen_server:call(?SERVER, get_nodes).

get_offline_nodes() ->
    gen_server:call(?SERVER, get_offline_nodes).

get_run_cmd_count(Node) ->
    gen_server:call(?SERVER, {get_run_cmd_count, Node}).

send_cmd_to_node(Cmd, Node) ->
    gen_server:cast(?SERVER, {send_cmd, Cmd, Node}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

node_full_name(Node, IP) ->
    list_to_atom(atom_to_list(Node) ++ "@" ++ IP).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{online_nodes=[], offline_nodes=[]}}.

handle_call({add_node, Node, IP}, _From, #state{online_nodes = OnlineNodes} = State) ->
    FullNode = node_full_name(Node, IP),
    case net_adm:ping(FullNode) of
        pong ->

    PID = beacon_echo:start(FullNode),
    monitor_node(FullNode, true),
    link(PID),
    io:format("!!!!!!!! new pid is ~p ~n", [PID]),
    {reply, ok, State#state{online_nodes = [{Node, FullNode, PID} | OnlineNodes]}};

handle_call({get_run_cmd_count, Node}, _From, #state{online_nodes = OnlineNodes} = State) ->
    Count = case lists:keysearch(Node, 1, OnlineNodes) of
                    {value, {Node, FullNode, _}} ->
                        beacon_echo:get_run_cmd_count(FullNode);
                    false ->
                        0
            end,
    {reply, Count, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_cmd, Node, Cmd}, #state{online_nodes = OnlineNodes} = State) ->
    case lists:keysearch(Node, 1, OnlineNodes) of
        {value, {Node, FullNode, _}} ->
            beacon_echo:run_cmd(FullNode, Cmd);
        false ->
            io:format("unknown node with name ~p ~n", [Node])
    end,
    {noreply, State}.

handle_info({nodedown, FullNode}, #state{online_nodes = OnlineNodes, offline_nodes = OfflineNodes} = State) ->
    NewState = case lists:keytake(FullNode, 2, OnlineNodes) of
                    {value, {Node, FullNode, _}, NewOnlineNodes} -> 
                        #state{online_nodes = NewOnlineNodes, offline_nodes = [{Node, FullNode} | OfflineNodes]};
                    false ->
                        State
                end,
    {noreply, NewState};

handle_info({'EXIT',Pid, Info}, #state{online_nodes = _OnlineNodes} = State) ->
    io:format("xxxxxxget exit from ~p ~p ~n", [Pid, Info]),
    %%NewState = case lists:keytake(Pid, 3, OnlineNodes) of
    %%                {value, {Node, FullNode, _}, NewOnlineNodes} -> 
    %%                    NewPid = spawn_link(FullNode, beacon_echo, start, []),
    %%                    State#state{online_nodes = [{Node, FullNode, NewPid} | NewOnlineNodes]};
    %%                false ->
    %%                    State
    %%            end,
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

