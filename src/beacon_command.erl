-module(beacon_command).
-include("beacon.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([create/3,
         parse_cmd/1,
         parse_cmd/2,
         to_json/1]).

-export([get_service/1,
         is_start_new_service/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
parse_cmd(CmdStr) ->
    parse_cmd(CmdStr, -1).

parse_cmd(CmdStr, AssignID) ->
    {struct, [{"name", Name}, {"args", {array, Args}}]} = mochijson:decode(CmdStr),
    create(AssignID, list_to_atom(Name), parse_args(Args)).

to_json(Cmd) ->
    #beacon_cmd{id = ID, name = Name, args = Args} = Cmd,
    mochijson:encode({struct, [{"id", ID}, 
                               {"name", Name}, 
                               {"args", {array, encode_args(Args)}}]}).
create(ID, Name, Args) ->
    #beacon_cmd{id = ID,
                name = Name,
                args = Args}.

get_service(Cmd) ->
    io:format("!!!! args ~p ~n", [Cmd#beacon_cmd.args]),
    {value, {"service", Service}} = lists:keysearch("service", 1, Cmd#beacon_cmd.args),
    Service.


is_start_new_service(Cmd) ->
    Cmd#beacon_cmd.name =:= 'start_service'.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%
%% args => {"args",
%%             {array,[{struct,[{"zone_name","www.1.com"}]},
%%                     {struct,[{"rr","1.1.1.1"}]}]}}]}
%%
parse_args(Args) ->
    lists:map(fun(ArgJsonStruct)-> {struct, [Arg]} = ArgJsonStruct, Arg end, Args).

encode_args(Args) ->
    lists:map(fun(ArgJson) -> {struct, [ArgJson]} end, Args).
