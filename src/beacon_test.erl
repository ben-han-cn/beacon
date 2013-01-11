-module(beacon_test).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([test/0]).


test() ->
    application:start(beacon),
    beacon_master:add_node(nana, "127.0.0.1"),
    Node = beacon_master:get_node(nana),
    beacon_slave_agent:start_service(Node, ruby, ["ruby /Users/hanfeng/workspace/code/erlang/beacon/src/test_port.rb"]),
    Ret = beacon_slave_agent:run_cmd(Node, ruby, sync_run, ["fucking good"]),
    io:format("xxxxxxx ret 1 ~p ~n", [Ret]),
    Ret2 = beacon_slave_agent:run_cmd(Node, ruby, sync_run, ["exit"]),
    io:format("xxxxxxx ret 1 ~p ~n", [Ret2]).
