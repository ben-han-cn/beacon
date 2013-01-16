-module(beacon_cmd_queue).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(START_ID, 1).
-include("beacon.hrl").
-record(state, {cmd_table,
                min_id,
                max_id}).
                

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/2,
         stop/1,
         enqueue/2,
         dequeue/1,
         dequeue_to_cmd/2,
         clear/1,
         get_cmd_id_range/1,
         reset/1,
         reset/2,
         to_list/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(QueueName, FilePath) ->
    {ok, PID} = gen_server:start_link({local, ?SERVER}, ?MODULE, [QueueName, FilePath], []),
    PID.

stop(Pid) ->
    gen_server:call(Pid, stop).

enqueue(Pid, Cmd) ->
    gen_server:call(Pid, {enqueue, Cmd}).

dequeue(Pid) ->
    gen_server:call(Pid, dequeue).

dequeue_to_cmd(Pid, CmdID) ->
    gen_server:call(Pid, {dequeue, CmdID}).

get_cmd_id_range(Pid) ->
    gen_server:call(Pid, get_cmd_id_range).

to_list(Pid) ->
    gen_server:call(Pid, to_list).

reset(Pid) ->
    reset(Pid, ?START_ID).

reset(Pid, ID) ->
    gen_server:call(Pid, {reset, ID}).

clear(Pid) ->
    gen_server:cast(Pid, clear).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([QueueName, FilePath]) ->
    {ok, QueueName} = dets:open_file(QueueName, [{file, FilePath}, {keypos, #beacon_cmd.id}]),
    CmdCount = dets:info(QueueName, size),
    case CmdCount of
        0 ->
            {ok, #state{cmd_table = QueueName, min_id = ?START_ID, max_id = ?START_ID}};
        _ ->
            MinID = mini_key_in_dets(QueueName),
            MaxID = MinID + CmdCount,
            {ok, #state{cmd_table = QueueName, min_id = MinID, max_id = MaxID}}
    end.


handle_call({enqueue, Cmd}, _From, #state{cmd_table = Table, max_id = MaxID} = State) ->
    CmdRecord = case is_record(Cmd, beacon_cmd) of
                    true -> 
                        MaxID = Cmd#beacon_cmd.id, %make sure insert valid id
                        Cmd;
                    false -> beacon_command:from_json(Cmd, MaxID)
                end,
    io:format("!!!! insert cmd ~p ~n", [CmdRecord]),
    ok = dets:insert(Table, CmdRecord),
    {reply, CmdRecord, State#state{max_id = MaxID + 1}};

handle_call(dequeue, _From, #state{cmd_table = Table, min_id = MinID} = State) ->
    [Cmd] = dets:lookup(Table, MinID),
    {reply, Cmd, State#state{min_id = MinID + 1}};

handle_call({dequeue_to_cmd, CmdID}, _From, #state{cmd_table = Table, min_id = MinID, max_id = MaxID} = State) when (CmdID > MinID) and (CmdID < MaxID)->
    NewMiniID = dequeue_cmd_helper(Table, MinID, CmdID),
    {reply, NewMiniID, State#state{min_id = NewMiniID}};

handle_call(get_cmd_id_range, _From, #state{max_id = MaxID, min_id = MinID} = State) ->
    {reply, [MinID, MaxID], State};

handle_call(to_list, _From, #state{cmd_table = Table, max_id = MaxID, min_id = MinID} = State) ->
    {reply, 
        lists:map(fun(ID) -> lists:nth(1, dets:lookup(Table, ID)) end, lists:seq(MinID, MaxID - 1)),
     State};

handle_call(stop, _From, #state{cmd_table = Table} = State) ->
    dets:close(Table),
    {stop, normal, ok, State};

handle_call({reset, ID}, _From, #state{cmd_table = Table} = State) ->
    ok = dets:delete_all_objects(Table),
    {reply, ok, State#state{min_id = ID, max_id= ID}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(clear, #state{cmd_table = Table, max_id = MaxID} = State) ->
    ok = dets:delete_all_objects(Table),
    {noreply, State#state{min_id = MaxID}};

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
mini_key_in_dets(Table) ->
    TmpEts = ets:new(tmp, [ordered_set, {keypos, #beacon_cmd.id}]),
    dets:to_ets(Table, TmpEts),
    ets:first(TmpEts).

dequeue_cmd_helper(Table,TargetMiniID, CurrentMiniID) when TargetMiniID =:= CurrentMiniID ->
    ok = dets:delete(Table, CurrentMiniID),
    CurrentMiniID + 1;
dequeue_cmd_helper(Table,TargetMiniID, CurrentMiniID) when TargetMiniID > CurrentMiniID ->
    ok = dets:delete(Table, CurrentMiniID),
    dequeue_cmd_helper(Table, TargetMiniID, CurrentMiniID + 1).

%% ------------------------------------------------------------------
%% Unit Test
%% ------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

cmd_queue_test() ->
    Queue = start_link(myqueue, "./queue_test"),
    reset(Queue),
    enqueue(Queue, "{\"name\":\"add zone1\", \"args\":[{\"zone_name\":\"www.1.com\"}, {\"rdata\":\"1.1.1.1\"}]}"),
    enqueue(Queue, "{\"name\":\"add zone2\", \"args\":[{\"zone_name\":\"www.2.com\"}]}"),
    enqueue(Queue, "{\"name\":\"add zone3\", \"args\":[{\"zone_name\":\"www.3.com\"}]}"),
    stop(Queue),

    Queue2 = start_link(myqueue, "./queue_test"),
    IDRange = get_cmd_id_range(Queue2),
    ?assertEqual([1, 4], IDRange),
    Cmds = to_list(Queue2),
    CmdsID = lists:map(fun(Cmd)-> Cmd#beacon_cmd.id end, Cmds),
    ?assertEqual(CmdsID, [1, 2, 3]),
    CmdsNames = lists:map(fun(Cmd)-> Cmd#beacon_cmd.name end, Cmds),
    ?assertEqual(CmdsNames, ['add zone1', 'add zone2', 'add zone3']),

    FirstCmd = dequeue(Queue2),
    ?assertEqual(FirstCmd#beacon_cmd.id, 1),
    {value, {"rdata", RdataArg}} = lists:keysearch("rdata", 1, FirstCmd#beacon_cmd.args),
    ?assertEqual(RdataArg, "1.1.1.1"),

    IDRangeNow = get_cmd_id_range(Queue2),
    ?assertEqual([2, 4], IDRangeNow),

    stop(Queue2).

-endif.
