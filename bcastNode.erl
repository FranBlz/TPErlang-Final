-module(bcastNode).
-include("struct.hrl").
-export([start/0, stop/0, pruebasFun/0]).
-export([preMonitor/0, senderFun/1, preListener/0, listenerFun/4, deliverFun/1, conectar/0, conectar/1, generate/2]).
-define(CREATE_MSG(Counter, Msg), #mcast{mid={node(), Counter + 1}, msg=Msg}).

-define(INIT_NODES, 6).

isNode(NodeName) ->
    0 < string:str(atom_to_list(NodeName), "atbcast").

getNodes() ->
    lists:filter(fun(Node) -> isNode(Node) end, nodes()).

getOthers() ->
    lists:filter(fun(Node) -> not isNode(Node) end, nodes()).

sendMsg(Msg, Nodes) ->
    lists:foreach(fun (Node) -> {listener, Node} ! Msg end, Nodes).

start() ->
    register(monitor, spawn(?MODULE, preMonitor,[])).

preMonitor() ->
    register(sender, spawn_link(?MODULE, senderFun,[0])),
    register(deliver, spawn_link(?MODULE, deliverFun,[dict:new()])),
    register(listener, spawn_link(?MODULE, preListener,[])),
    register(pruebas, spawn_link(?MODULE, pruebasFun,[])),
    monitorFun().
monitorFun() ->
    receive
        {'ERROR', _SenderID, _Reason} ->
            io:format("Lo catcheamos pa~n")
    end.

stop() ->
    fin.

pruebasFun() ->
    receive
        A -> 
            case is_record(A, send) of
                true -> xd
            end
    end.

%% Beginning of sender section
senderFun(Counter) ->
    receive
        M when is_record(M, send) ->
            Msg = ?CREATE_MSG(Counter, M#send.msg),
            listener ! Msg,
            sendMsg(Msg, getNodes()),
            senderFun(Counter + 1);
        {rip} -> 
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
        {rip} ->
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

propBigger({CNum, CProp, _}, {Num, Prop, _}) ->
    if
        CNum == Num -> CProp > Prop;
        true -> CNum > Num
    end.

%% Beginning of listener section
insertar([], Record) -> [Record];
insertar([CMsg | Tl], Msg) ->
    case propBigger(CMsg, Msg) of
        true ->
            [Msg | [CMsg | Tl]];
        false ->
            [CMsg|insertar(Tl, Msg)]
    end.

preListener() ->
    net_kernel:monitor_nodes(true),
    listenerFun(0, dict:new(), [], infinity).

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
            case dict:is_empty(Pend) of
                true ->
                    NewTO = infinity;
                false ->
                    NewTO = 0
            end,
            listenerFun(
                max(Proposal, M#result.hprop),
                dict:erase(M#result.mid, Pend),
                insertar(Defin, Msg), NewTO);
                % insertar(Defin, {Msg, M#result.proposer}), NewTO);
        {nodedown, Node} ->
            case isNode(Node) of
                true ->
                    IsLessHalf = 2*length(getNodes()) =< ?INIT_NODES,
                    case IsLessHalf of
                        true ->
                            sender ! {rip},
                            deliver ! {rip},
                            sendMsg({serverdown}, getOthers()),
                            erlang:halt();
                        false ->
                            sendMsg({nodedown}, getOthers()),
                            listenerFun(Proposal, Pend, Defin, TO)
                    end;
                false ->
                    listenerFun(Proposal, Pend, Defin, TO)
            end
    after TO ->
        case Defin of
            [Msg|Tl] ->
                %{Mens, Prop} = Msg,
                %io:format("~p ~p ~n", [Mens, Prop]),
                sendMsg({res, Msg}, getOthers()),
                listenerFun(Proposal, Pend, Tl, 0);
            [] ->
                listenerFun(Proposal, Pend, [], infinity)
        end
    end.
%% End of listener section

generate(_TO, 0) -> finished;
generate(TO, N) ->
    timer:sleep(TO),
    sender ! #send{msg = TO},
    generate(round(rand:uniform()*10000), N - 1).

conectar(0) ->
    listo;
conectar(N) ->
    net_adm:ping(list_to_atom("node" ++ [49+N] ++ "@dani-pc")),
    conectar(N-1).
conectar() ->
    conectar(6).
