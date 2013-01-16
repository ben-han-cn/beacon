-module(beacon_command).
-include("beacon.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([create/3,
         from_json/1,
         from_json/2,
         to_json/1]).

-export([get_service/1,
         is_start_new_service/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
from_json(CmdStr) ->
    from_json(CmdStr, -1).

from_json(CmdJson, AssignID) ->
    {struct, [{"name", Name}, {"args", {array, Args}}]} = mochijson:decode(CmdJson),
    create(AssignID, list_to_atom(Name), decode_args(Args)).

to_json(Cmd) ->
    #beacon_cmd{name = Name, args = Args} = Cmd,
    mochijson:encode({struct, [{"name", Name}, 
                               {"args", {array, encode_args(Args)}}]}).
create(ID, Name, Args) ->
    #beacon_cmd{id = ID,
                name = Name,
                args = Args}.

get_service(Cmd) ->
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
decode_args(Args) ->
    lists:map(fun(ArgJsonStruct)-> {struct, [Arg]} = ArgJsonStruct, Arg end, Args).

encode_args(Args) ->
    lists:map(fun(ArgJson) -> {struct, [ArgJson]} end, Args).
