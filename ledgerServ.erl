-module(ledgerServ).
-include("struct.hrl").
-export([start/0, listenerFun/3]).

%% conectar cada servidor ledger a un nodo manualmente mediante hidden
start() ->
    register(listener, spawn(?MODULE, listenerFun,[[], [], []])).

listenerFun(Ledger, GetPend, AppPend) ->
    receive
        {get, Who, Counter} ->
            lists:foreach(fun(X) -> {sender, X} ! #send{msg = {get, Who, Counter}, sender = self()} end, nodes(hidden)),
            listenerFun(Ledger, [{Who, Counter} | GetPend], AppPend);
        {app, Who, Counter, Value} ->
            lists:foreach(fun(X) -> {sender, X} ! #send{msg = {app, Who, Counter, Value}, sender = self()} end, nodes(hidden)),
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
                    listenerFun(Ledger, GetPend, AppPend)
            end
    end.