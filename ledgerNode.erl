-module(ledgerNode).
-include("struct.hrl").
-export([start/0, stop/0]).
-export([senderFun/1, preListener/4, listenerFun/4, deliverFun/1, conectar/0, conectar/1]).
-define(CREATE_MSG(Counter, Msg), #mcast{mid={node(), Counter + 1}, msg=Msg}).

-define(MAX_NODES, 6).

isNode(NodeName) ->
    0 < string:str(atom_to_list(NodeName), "node").

isLedger(NodeName) ->
    0 < string:str(atom_to_list(NodeName), "ledger").

getNodes() ->
    lists:filter(fun(Node) -> isNode(Node) end, nodes()).

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
            lists:foreach(fun(Node) -> {listener, Node} ! Msg end, getNodes()),
            senderFun(Counter + 1);
        rip ->
            fin;
        _ ->
            io:format("Invalid msg ~n"),
            senderFun(Counter)
    end.
%% End of sender section

%% Mid #{Hprop, Proposer, Nrep}
%% Mid = {node, sn}
deliverFun(Dicc) ->
    receive
        rip ->
            fin;
        M when is_record(M, rep) ->
            case dict:find(M#rep.mid, Dicc) of
                {ok, [{Hprop, Proposer, Nrep}]} ->
                    Total = length(nodes()),
                    if
                        Nrep + 1 == Total ->
                            listener ! #result{mid = M#rep.mid, hprop = max(Hprop, M#rep.hprop), proposer = max(Proposer, M#rep.proposer)},
                            lists:foreach(fun(X) -> {listener, X} ! #result{mid = M#rep.mid, hprop = max(Hprop, M#rep.hprop), proposer = max(Proposer, M#rep.proposer)} end, lists:filter(fun(X) -> isNode(X) end, nodes())),
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

preListener(S, Pend, Defin, TO) ->
    net_kernel:monitor_nodes(true),
    listenerFun(S, Pend, Defin, TO).

listenerFun(S, Pend, Defin, TO) ->
    receive
        M when is_record(M, mcast) ->
            {Sender, _Counter} = M#mcast.mid,
            {deliver, Sender} ! #rep{mid = M#mcast.mid, hprop = S + 1, proposer = node()},
            listenerFun(S + 1, dict:append(M#mcast.mid, M#mcast.msg, Pend), Defin, infinity);
        M when is_record(M, result) ->
            {ok, [Msg]} = dict:find(M#result.mid, Pend),
            listenerFun(max(S, M#result.hprop), dict:erase(M#result.mid, Pend), insertar(Defin, Msg), 0);
        {nodedown, Node} ->
            io:format("Buenas~n"),
            case isNode(Node) of
                true ->
                    Total = 2*length(lists:filter(fun(X) -> isNode(X) end, nodes())),
                    if
                        Total =< ?MAX_NODES ->
                            sender ! rip,
                            deliver ! rip,
                            lists:foreach(fun(X) -> {listener, X} ! {serverdown} end, lists:filter(fun(X) -> isLedger(X) end, nodes())),
                            erlang:halt();
                        true ->
                            lists:foreach(fun(X) -> {listener, X} ! {nodedown} end, lists:filter(fun(X) -> isLedger(X) end, nodes())),
                            listenerFun(S, Pend, Defin, TO)
                    end;
                false ->
                    listenerFun(S, Pend, Defin, TO)
            end
    after TO ->
        case Defin of
            [{get, Who, Counter} | Tl] ->
                lists:foreach(fun(X) -> {listener, X} ! {getRes, Who, Counter} end, lists:filter(fun(X) -> isLedger(X) end, nodes())),
                listenerFun(S, Pend, Tl, 0);
            [{app, Who, Counter, Value} | Tl] ->
                lists:foreach(fun(X) -> {listener, X} ! {appRes, Who, Counter, Value} end, lists:filter(fun(X) -> isLedger(X) end, nodes())),
                listenerFun(S, Pend, Tl, 0);
            [] ->
                listenerFun(S, Pend, [], infinity)
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
