-module(ledgerCli).
-export([start/0, get/0, append/1, senderFun/1, preListener/0]).

% Starts necessary processes for client
start() ->
    register(sender, spawn(?MODULE, senderFun, [0])),
    register(listener, spawn(?MODULE, preListener,[])).

% Handles requests from user and delivers them to the connected ledger server
senderFun(C) ->
    receive
        {get} ->
            lists:foreach(fun(X) -> {listener, X} ! {get, node(), C + 1} end, nodes()),
            senderFun(C + 1);
        {append, Value} ->
            lists:foreach(fun(X) -> {listener, X} ! {app, node(), C + 1, Value} end, nodes()),
            senderFun(C + 1)
    end.

% User functions for showing and adding elements to the ledger
get() ->
    sender ! {get},
    ok.
append(Value) -> 
    sender ! {append, Value},
    ok.

% Handles results coming from ledger server and notifies the user necesarry information about the service
preListener() ->
    net_kernel:monitor_nodes(true),
    listenerFun().
listenerFun() ->
    receive
        {getRes, Counter, Ledger} ->
            io:format("~p ~p ~n", [Counter, Ledger]),
            listenerFun();
        {nodedown, _Node} ->
            io:format("Cayó el servidor ledger~n"),
            listenerFun();
        {rip} ->
            io:format("Cayó el servicio bcast~n");
        {nottrusty} ->
            io:format("Cayó un nodo del servicio broadcast~n"),
            listenerFun()
    end.