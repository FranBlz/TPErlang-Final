-module(ledgerCli).
-export([start/0, getFun/0, appendFun/1, listenerFun/0, senderFun/1]).

start() ->
    register(sender, spawn(?MODULE, senderFun, [0])),
    register(listener, spawn(?MODULE, listenerFun,[])).

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

listenerFun() ->
    receive
        {getRep, Counter, Ledger} ->
            io:format("~p ~p ~n", [Counter, Ledger])
    end.