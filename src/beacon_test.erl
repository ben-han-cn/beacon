-module(beacon_test).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([test/0]).


test() ->
    application:start(beacon),
    {ok, Node} = beacon_master:add_node(nana, "127.0.0.1"),
    beacon_slave_agent:run_critical_cmd(Node, 
        "{\"name\" : \"start_service\", \"args\": {\"service\": \"ruby\", \"shell\": \"ruby /Users/hanfeng/workspace/code/erlang/beacon/src/test_port.rb\"}}"),
    lists:foreach(fun(I) ->
        beacon_slave_agent:run_critical_cmd(Node, "{\"name\" : \"echo\", \"args\": {\"service\": \"ruby\", \"param\": \"" ++ "a" ++ integer_to_list(I) ++ "\"}}")
        end, lists:seq(1, 2000)),
    beacon_slave_agent:run_critical_cmd(Node, "{\"name\" : \"echo\", \"args\": {\"service\": \"ruby\", \"param\": \"exit\"}}").
