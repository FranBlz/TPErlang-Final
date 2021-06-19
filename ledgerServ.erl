-module(ledgerServ).
-include("struct.hrl").
-export([start/0, preListener/0]).

-define(MAX_NODES, 6).
%% conectar cada servidor ledger a un nodo manualmente mediante hidden
start() ->
    register(listener, spawn(?MODULE, preListener,[])).

isNode(NodeName) ->
    0 < string:str(atom_to_list(NodeName), "node").

preListener() ->
    net_kernel:monitor_nodes(true),
    listenerFun([], [], []).
listenerFun(Ledger, GetPend, AppPend) ->
    receive
        {get, Who, Counter} ->
            lists:foreach(fun(X) -> {sender, X} ! #send{msg = {get, Who, Counter}, sender = node()} end, lists:filter(fun(X) -> isNode(X) end, nodes())),
            listenerFun(Ledger, [{Who, Counter} | GetPend], AppPend);
        {app, Who, Counter, Value} ->
            lists:foreach(fun(X) -> {sender, X} ! #send{msg = {app, Who, Counter, Value}, sender = node()} end, lists:filter(fun(X) -> isNode(X) end, nodes())),
            listenerFun(Ledger, GetPend, [{Who, Counter} | AppPend]);
        {getRes, Who, Counter} ->
            IsMember = lists:member({Who, Counter}, GetPend),
            if
                IsMember ->
                    {listener, Who} ! {getRes, Counter, Ledger},
                    listenerFun(Ledger, lists:delete({Who, Counter}, GetPend), AppPend);
                true ->
                    listenerFun(Ledger, GetPend, AppPend)
            end;
        {appRes, Who, Counter, Value} ->
            IsMember = lists:member({Who, Counter}, GetPend),
            if
                IsMember ->
                    listenerFun(Ledger ++ [Value], GetPend, lists:delete({Who, Counter}, AppPend));
                true ->
                    listenerFun(Ledger ++ [Value], GetPend, AppPend)
            end;
        {serverdown} ->
            lists:foreach(fun(X) -> {listener, X} ! {rip} end, lists:filter(fun(X) -> not isNode(X) end, nodes())),
            erlang:halt();
        {nodedown} ->
            lists:foreach(fun(X) -> {listener, X} ! {nottrusty} end, lists:filter(fun(X) -> not isNode(X) end, nodes())),
            listenerFun(Ledger, GetPend, AppPend);
        {nodedown, Node} ->
            case isNode(Node) of
                true ->
                    case reconnect(?MAX_NODES) of
                        serverdown -> 
                            lists:foreach(fun(X) -> {listener, X} ! {rip} end, lists:filter(fun(X) -> not isNode(X) end, nodes()));
                        done ->
                            listenerFun(Ledger, GetPend, AppPend)
                    end;
                false ->
                    listenerFun(Ledger, GetPend, AppPend)
            end     
    end.

reconnect(0) ->
    serverdown;
reconnect(N) ->
    case net_kernel:connect_node(list_to_atom(lists:concat([node,N,'@FranPC']))) of
        true -> 
            done;
        false ->
            reconnect(N - 1)
    end.