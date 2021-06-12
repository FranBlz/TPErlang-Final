-module(conectar).
-export([con/0]).

con() ->
    net_kernel:connect_node(node1@FranPC),
    %rpc:call(node1@FranPC, node, start, []),
    net_kernel:connect_node(node2@FranPC),
    net_kernel:connect_node(node3@FranPC).