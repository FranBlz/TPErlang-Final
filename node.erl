-module(node).
-include("struct.hrl").
-export([start/0, stop/0, send/1, senderFun/0, communicatorFun/2, listenerFun/3, deliverFun/3]).

%% send envia definitivo, deliver (escribe y escucha) consenso, listener escuche mensaje

start() ->
    register(sender, spawn(?MODULE, senderFun,[])),
    register(deliver, spawn(?MODULE, deliverFun,[0, 0, 0])),
    register(listener, spawn(?MODULE, listenerFun,[0, dict:new(), infinity])),
    register(communicator, spawn(?MODULE, communicatorFun,[0, ""])).

stop() -> 
    fin.

%% Beginning of sender section
send(Msg) ->
    sender ! #send{msg = Msg, sender = self()},
    ok.

senderFun() ->
    receive
        M when is_record(M, send) ->
            communicator ! {add, M#send.msg},
            senderFun();
        _ ->
            io:format("Recv cualca ~n"),
            senderFun()
    end.

communicatorFun(TO, CurrentMsg) ->
    receive
        {prop, Num} ->
            listener ! #msg{body = CurrentMsg, sender = self(), sn = Num},
            lists:foreach(fun(X) -> {listener, X} ! #msg{body = CurrentMsg, sender = self(), sn = Num} end, nodes()),
            communicatorFun(0,"")
    after TO -> 
        receive
            {add, Msg} ->
                deliver ! get_proposal,
                communicatorFun(infinity, Msg)
        end
    end.
%% End of sender section

%% Beginning of deliver section
maximum(V1, V2) when V1 >= V2 ->
    V1;
maximum(_V1, V2) ->
    V2.

deliverFun(Num, Hprop, Nrep) ->
    receive
        get_proposal ->
            lists:foreach(fun(X) -> {deliver, X} ! #prop{sender = self(), sn = Num} end, nodes()),
            deliverFun(Num + 1, Num, 1);
        M when is_record(M, prop) ->
            M#prop.sender ! #rep{sender = self(), sn = Num},
            deliverFun(Num, Hprop, Nrep);
        M when is_record(M, rep) ->
            if 
                Nrep + 1 == 2 ->
                    communicator ! {prop, maximum(M#rep.sn, Hprop)},
                    deliverFun(Num, Num, 0);
                true ->
                    deliverFun(Num, maximum(M#rep.sn, Hprop), Nrep + 1)
            end
    end.
%% End of deliver section

%% Beginning of listener section
listenerFun(N, Pend, TO) ->
    receive 
        M when is_record(M, msg) ->
            listenerFun(N, dict:append(M#msg.sn, {M#msg.body}, Pend), 0)
    after TO ->
        case dict:find(N, Pend) of
                {ok, [Value|_Tl]} ->
                    io:format("Deliver : ~p ~p ~n", [Value, N]),
                    listenerFun(N + 1, Pend, 0);
                error ->
                    listenerFun(N, Pend, infinity)
        end
    end.
%% End of listener section