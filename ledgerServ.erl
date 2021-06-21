-module(ledgerServ).
-include("struct.hrl").
-export([start/0, preListener/0]).

-define(MAX_NODES, 6).

start() ->
    register(listener, spawn(?MODULE, preListener,[])).

isNode(NodeName) ->
    0 < string:str(atom_to_list(NodeName), "atbcast").

getNodes() ->
    lists:filter(fun(X) -> isNode(X) end, nodes()).

getOthers() ->
    lists:filter(fun(X) -> not isNode(X) end, nodes()).

sendMsg(Msg, Nodes) ->
    lists:foreach(fun(X) -> {sender, X} ! #send{msg = Msg} end, Nodes).

preListener() ->
    net_kernel:monitor_nodes(true),
    listenerFun([], [], []).
listenerFun(Ledger, GetPend, AppPend) ->
    receive
        {get, Client, Counter} ->
            sendMsg({get, Client, Counter}, getNodes()),
            listenerFun(Ledger, [{Client, Counter} | GetPend], AppPend);
        {app, Client, Counter, Value} ->
            sendMsg({app, Client, Counter, Value}, getNodes()),
            listenerFun(Ledger, GetPend, [{Client, Counter} | AppPend]);
        {res, {get, Client, Counter}} ->
            MyMsg = lists:member({Client, Counter}, GetPend),
            if
                MyMsg ->
                    {listener, Client} ! {getRes, Counter, Ledger},
                    listenerFun(Ledger, lists:delete({Client, Counter}, GetPend), AppPend);
                true ->
                    listenerFun(Ledger, GetPend, AppPend)
            end;
        {res, {app, Client, Counter, Value}} ->
            MyMsg = lists:member({Client, Counter}, GetPend),
            if
                MyMsg ->
                    listenerFun(Ledger ++ [Value], GetPend, lists:delete({Client, Counter}, AppPend));
                true ->
                    listenerFun(Ledger ++ [Value], GetPend, AppPend)
            end;
        {serverdown} ->
            sendMsg({rip}, getOthers()),
            erlang:halt();
        {nodedown} ->
            sendMsg({nottrusty}, getOthers()),
            listenerFun(Ledger, GetPend, AppPend);
        {nodedown, Node} ->
            case isNode(Node) of
                true ->
                    case reconnect(?MAX_NODES, list_to_atom(string:find(atom_to_list(Node), "@"))) of
                        serverdown -> 
                            sendMsg({rip}, getOthers());
                        done ->
                            listenerFun(Ledger, GetPend, AppPend)
                    end;
                false ->
                    listenerFun(Ledger, GetPend, AppPend)
            end     
    end.

reconnect(0, _Sufix) ->
    serverdown;
reconnect(N, Sufix) ->
    case net_kernel:connect_node(list_to_atom(lists:concat([node,N,Sufix]))) of
        true -> 
            done;
        false ->
            reconnect(N - 1, Sufix)
    end.