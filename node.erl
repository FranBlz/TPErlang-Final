-module(dest).
-include("struct.hrl").
-export([start/0, stop/0]).

%% send envia definitivo, deliver (escribe y escucha) consenso, listener escuche mensaje

start() ->
    register(sender, spawn(?MODULE, senderFun,[[]])),
    register(deliver, spawn(?MODULE, deliverFun,[])),
    register(listener, spawn(?MODULE, listenerFun,[])),
    register(communicator, spawn(?MODULE, communicatorFun,[])).

stop() -> 
    fin.

%% Beginning of sender section
send(Msg) ->
    sender ! #send{msg = Msg, sender = self()},
    ok.

senderFun(Cola) ->
    receive
        M when is_record(M, send) ->
            communicator ! {add, M}
        _ ->
            io:format("Recv cualca ~n")
    end.

communicator(Cola) ->
    receive
        {add, Msg} ->
            if 
                Cola == [] -> deliver ! start;
                _ -> ok
            end,
            communicator(Cola ++ [{Msg, -1}]);
        {prop, Num} -> 
            case Cola of
                [Msg|Tl] ->
                    listener ! {Msg, Num},
                    lists:foreach(fun(Node) -> {listener, Node} ! {Msg, Num} end, nodes())
    end.
%% End of sender section

%% Beginning of deliver section
%% End of deliver section

%% Beginning of listener section

listener() ->
    receive
        M when is_record(M, prop) ->
            deliver ! prop
            
        M when is_record(M, msg) ->
            ;
        Propuesta ->
            max(Propuesta, deliver)
        {ok, Prop} -> M#prop.sender ! Prop    
    end.

%% End of listener section

destination(diccionario)
    receive
        {prop, }
