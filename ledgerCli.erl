-module(ledgerCli).
-export([start/0, getFun/0, appendFun/1, listenerFun/0, senderFun/1, preListener/0]).

start() ->
    register(sender, spawn(?MODULE, senderFun, [0])),
    register(listener, spawn(?MODULE, preListener,[])).

senderFun(C) ->
    receive
        {get} ->
            lists:foreach(fun(X) -> {listener, X} ! {get, node(), C + 1} end, nodes()),
            senderFun(C + 1);
        {append, Value} ->
            lists:foreach(fun(X) -> {listener, X} ! {app, node(), C + 1, Value} end, nodes()),
            senderFun(C + 1)
    end.

getFun() ->
    sender ! {get}.

appendFun(Value) -> 
    sender ! {append, Value}.

preListener() ->
    net_kernel:monitor_nodes(true),
    listenerFun().
listenerFun() ->
    receive
        {getRes, Counter, Ledger} ->
            io:format("~p ~p ~n", [Counter, Ledger]),
            listenerFun();
        {nodedown, _Node} ->
            io:format("Ocurrio un error en el servidor ledger~n"),
            listenerFun();
        {rip} ->
            io:format("Ocurrio un error en el servicio bcast~n");
        {nottrusty} ->
            io:format("El servicio bcast ya no es confiable~n"),
            listenerFun()
    end.