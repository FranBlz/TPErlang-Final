-module(node).
-include("struct.hrl").
-export([start/0, stop/0, send/1, generate/2]).
-export([senderFun/1, listenerFun/4, deliverFun/1]).

-define(MAX_NODES, 10).

start() ->
    register(sender, spawn(?MODULE, senderFun,[0])),
    register(deliver, spawn(?MODULE, deliverFun,[dict:new()])),
    register(listener, spawn(?MODULE, listenerFun,[0, dict:new(), [], infinity])).

stop() ->
    fin.

%% Beginning of sender section
send(Msg) ->
    sender ! #send{msg = Msg, sender = node()},
    ok.

senderFun(Counter) ->
    receive
        M when is_record(M, send) ->
            listener ! #mcast{mid = {M#send.sender, Counter + 1}, msg = M#send.msg},
            lists:foreach(fun(X) -> {listener, X} ! #mcast{mid = {M#send.sender, Counter + 1}, msg = M#send.msg} end, nodes()),
            senderFun(Counter + 1);
        _ ->
            io:format("Invalid msg ~n"),
            senderFun(Counter)
    end.
%% End of sender section

%% Beginning of deliver section
maximum(V1, V2) when V1 >= V2 ->
    V1;
maximum(_V1, V2) ->
    V2.

%% Mid #{Hprop, Proposer, Nrep}
%% Mid = {node, sn}
deliverFun(Dicc) ->
    receive
        M when is_record(M, rep) ->
            case dict:find(M#rep.mid, Dicc) of
                {ok, [{Hprop, Proposer, Nrep}]} ->
                    Total = length(nodes()),
                    if
                        Nrep + 1 >= Total ->
                            listener ! #result{mid = M#rep.mid, hprop = maximum(Hprop, M#rep.hprop), proposer = maximum(Proposer, M#rep.proposer)},
                            lists:foreach(fun(X) -> {listener, X} ! #result{mid = M#rep.mid, hprop = maximum(Hprop, M#rep.hprop), proposer = maximum(Proposer, M#rep.proposer)} end, nodes()),
                            deliverFun(dict:erase(M#rep.mid, Dicc));
                        Hprop < M#rep.hprop ->
                            deliverFun(dict:update(M#rep.mid, fun (_) -> [{M#rep.hprop, M#rep.proposer, Nrep + 1}] end, Dicc));
                        Hprop == M#rep.hprop and (M#rep.proposer < Proposer) ->
                            deliverFun(dict:update(M#rep.mid, fun (_) -> [{M#rep.hprop, M#rep.proposer, Nrep + 1}] end, Dicc));
                        true ->
                            deliverFun(dict:update(M#rep.mid, fun (_) -> [{Hprop, Proposer, Nrep + 1}] end, Dicc))
                    end;
                error -> 
                    deliverFun(dict:append(M#rep.mid, {M#rep.hprop, M#rep.proposer, 1}, Dicc))
            end
    end.
%% End of deliver section



%% Beginning of listener section
insertar([], Tupla) -> [Tupla];
insertar([{CProposal, CProposer, CMsg} | Tl], {Proposal, Proposer, Msg})
    when CProposal < Proposal ->
        [{CProposal, CProposer, CMsg} | insertar(Tl, {Proposal, Proposer, Msg})];
insertar([{CProposal, CProposer, CMsg} | Tl], {Proposal, Proposer, Msg})
    when CProposal > Proposal ->
        [{Proposal, Proposer, Msg} | [{CProposal, CProposer, CMsg} | Tl]];
insertar([{CProposal, CProposer, CMsg} | Tl], {Proposal, Proposer, Msg})
    when CProposer < Proposer ->
        [{CProposal, CProposer, CMsg} | insertar(Tl, {Proposal, Proposer, Msg})];
insertar([CProp | Tl], Prop) ->
    [Prop|[CProp|Tl]].

listenerFun(S, Pend, Defin, TO) ->
    receive
        M when is_record(M, mcast) ->
            {Sender, _Counter} = M#mcast.mid,
            {deliver, Sender} ! #rep{mid = M#mcast.mid, hprop = S + 1, proposer = node()},
            listenerFun(S + 1, dict:append(M#mcast.mid, M#mcast.msg, Pend), Defin, infinity);
        M when is_record(M, result) ->
            {ok, [Msg]} = dict:find(M#result.mid, Pend),
            listenerFun(maximum(S, M#result.hprop), dict:erase(M#result.mid, Pend), insertar(Defin, {M#result.hprop, M#result.proposer, Msg}), 0)
    after TO ->
        case Defin of
            [{Hprop, Proposer, Msg} | Tl] ->
                io:format("Deliver: ~p ~p ~p ~n", [Hprop, Proposer, Msg]),
                listenerFun(S, Pend, Tl, 0);
            [] ->
                listenerFun(S, Pend, [], infinity)
        end
    end.
%% End of listener section

generate(_TO, 0) -> finished;
generate(TO, N) ->
    timer:sleep(TO),
    send(TO),
    generate(round(rand:uniform()*10000), N - 1).