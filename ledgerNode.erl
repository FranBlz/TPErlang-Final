-module(ledgerNode).
-include("struct.hrl").
-export([start/0, stop/0]).
-export([senderFun/1, preListener/4, listenerFun/4, deliverFun/1, conectar/0, conectar/1]).
-define(CREATE_MSG(Counter, Msg), #mcast{mid={node(), Counter + 1}, msg=Msg}).

-define(INIT_NODES, 6).

isNode(NodeName) ->
    0 < string:str(atom_to_list(NodeName), "node").

getNodes() ->
    lists:filter(fun(Node) -> isNode(Node) end, nodes()).

getOthers() ->
    lists:filter(fun(Node) -> not isNode(Node) end, nodes()).

sendMsg(Msg, Nodes) ->
    lists:foreach(fun (Node) -> {listener, Node} ! Msg end, Nodes).

start() ->
    register(sender, spawn(?MODULE, senderFun,[0])),
    register(deliver, spawn(?MODULE, deliverFun,[dict:new()])),
    register(listener, spawn(?MODULE, preListener,[0, dict:new(), [], infinity])).

stop() ->
    fin.

%% Beginning of sender section
senderFun(Counter) ->
    receive
        M when is_record(M, send) ->
            Msg = ?CREATE_MSG(Counter, M#send.msg),
            listener ! Msg,
            sendMsg(Msg, getNodes()),
            senderFun(Counter + 1);
        rip ->
            fin;
        _ ->
            io:format("Invalid msg ~n"),
            senderFun(Counter)
    end.
%% End of sender section

getProp(0, New, Old) -> max(New, Old);
getProp(Rest, New, _Old) when Rest > 0 -> New;
getProp(_Rest, _New, Old) -> Old.

% %% Mid #{Hprop, Proposer, Nrep}
% %% Mid = {node, sn}
deliverFun(Dict) ->
    receive
        rip ->
            fin;
        #rep{mid = MsgId, hprop = NewHProp, proposer = NewProposer} ->
            case dict:find(MsgId, Dict) of
                {ok, [{OldHProp, OldProposer, Nrep}]} ->
                    HProp = max(NewHProp, OldHProp),
                    Proposer = getProp(NewHProp - OldHProp, NewProposer, OldProposer),
                    Last = (Nrep + 1) == length(getNodes()),
                    case Last of
                        true ->
                            Res =  #result{mid = MsgId, hprop = HProp, proposer = Proposer},
                            listener ! Res,
                            sendMsg(Res, getNodes()),
                            deliverFun(dict:erase(MsgId, Dict));
                        false ->
                            deliverFun(dict:update(MsgId, fun (_) -> [{HProp, Proposer, Nrep + 1}] end, Dict))
                    end;
                error -> 
                    deliverFun(dict:append(MsgId, {NewHProp, NewProposer, 0}, Dict))
            end
    end.

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

preListener(S, Pend, Defin, TO) ->
    net_kernel:monitor_nodes(true),
    listenerFun(S, Pend, Defin, TO).


listenerFun(Proposal, Pend, Defin, TO) ->
    receive
        M when is_record(M, mcast) ->
            {Sender, _Counter} = M#mcast.mid,
            {deliver, Sender} ! #rep{mid = M#mcast.mid, hprop = Proposal + 1, proposer = node()},
            listenerFun(
                Proposal + 1,
                dict:append(M#mcast.mid, M#mcast.msg, Pend),
                Defin, infinity);
        M when is_record(M, result) ->
            {ok, [Msg]} = dict:find(M#result.mid, Pend),
            listenerFun(
                max(Proposal, M#result.hprop), %Esto esta bien?
                dict:erase(M#result.mid, Pend),
                insertar(Defin, Msg), 0);  %Creo que no cambia si no estan ordenados
        {nodedown, Node} ->
            case isNode(Node) of
                true ->
                    IsLessHalf = 2*length(getNodes()) =< ?INIT_NODES,
                    case IsLessHalf of
                        true ->
                            sender ! rip,
                            deliver ! rip,
                            sendMsg({serverdown}, getOthers()),
                            erlang:halt();
                        true ->
                            sendMsg({nodedown}, getOthers()),
                            listenerFun(Proposal, Pend, Defin, TO)
                    end;
                false ->
                    listenerFun(Proposal, Pend, Defin, TO)
            end
    after TO ->
        case Defin of
            [Msg|Tl] ->
                sendMsg({res, Msg}, getOthers()),
                listenerFun(Proposal, Pend, Tl, 0);
            [] ->
                listenerFun(Proposal, Pend, [], infinity)
        end
    end.
%% End of listener section


conectar(0) ->
    listo;
conectar(N) ->
    net_adm:ping(list_to_atom("node" ++ [49+N] ++ "@dani-pc")),
    conectar(N-1).
conectar() ->
    conectar(6).
