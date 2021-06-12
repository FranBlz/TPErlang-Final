-module(conectar).
-export([con/0]).

con() ->
    net_kernel:connect_node(node1@FranPC),
    %rpc:call(node1@FranPC, node, start, []),
    rpc:call(node2@FranPC, node, start, []),
    rpc:call(node3@FranPC, node, start, []).