-module(node).
-include("struct.hrl").
-export([start/0, stop/0, send/1, generate/2]).
-export([senderFun/1, listenerFun/4, deliverFun/1]).
-define(CREATE_MSG(Msg, N), #mcast{mid={Msg#send.sender, N+1}, msg=Msg#send.msg}).
-define(CREATE_RES(MsgId, Hprop, Prop), #result{mid=MsgId, hprop=Hprop, proposer=Prop}).
%-define(MAX_NODES, 4).
%% send envia definitivo, deliver (escribe y escucha) consenso, listener escuche mensaje
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

%% * Renombrar a ABroadcast segun paper
senderFun(Cant) ->
    receive
        M when is_record(M, send) ->
            Msg = ?CREATE_MSG(M, Cant),
            listener ! Msg,
            lists:foreach(fun(Node) -> {listener, Node} ! Msg end, nodes()),
            senderFun(Cant + 1);
        _ ->
            io:format("Invalid msg ~n"),
            senderFun(Cant)
    end.
%% End of sender section

getProp(OldHprop, NewHprop, _, NewProp) when OldHprop < NewHprop -> NewProp;
getProp(OldHprop, NewHprop, OldProp, _) when OldHprop > NewHprop -> OldProp;
getProp(_, _, OldProp, NewProp) when OldProp < NewProp -> NewProp;
getProp(_, _, OldProp, _) -> OldProp.

%% Mid #{Hprop, Proposer, Nrep}
%% Mid = {node, sn}
%% * Renombrar a ADeliver segun paper
deliverFun(MsgDict) ->
    receive
        #rep{mid = MsgId, hprop = NewHprop, proposer = NewProposer} ->
            case dict:find(MsgId, MsgDict) of
                {ok, [{OldHprop, OldProposer, Nrep}]} ->
                    HProp = max(OldHprop, NewHprop),
                    Prop = getProp(OldHprop, NewHprop, OldProposer, NewProposer),
                    Cant = Nrep + 1,
                    Total = length(nodes()),
                    if
                        Cant == Total ->
                            Res = ?CREATE_RES(MsgId, HProp, Prop),
                            listener ! Res,
                            lists:foreach(fun(Node) -> {listener, Node} ! Res end, nodes()),
                            deliverFun(dict:erase(MsgId, MsgDict));
                        true ->
                            deliverFun(dict:update(MsgId, fun (_) -> [{HProp, Prop, Cant}] end, MsgDict))
                    end;
                error -> 
                    deliverFun(dict:append(MsgId, {NewHprop, NewProposer, 1}, MsgDict))
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
            listenerFun(max(S, M#result.hprop), dict:erase(M#result.mid, Pend), insertar(Defin, {M#result.hprop, M#result.proposer, Msg}), 0)
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