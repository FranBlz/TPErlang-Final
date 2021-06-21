-module(conectar).
-export([con/1]).

con(0) -> done;
con(N) ->
    net_kernel:connect_node(list_to_atom(lists:concat([node,N,'@FranPC']))),
    con(N - 1).