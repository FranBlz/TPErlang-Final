-module(bcastNode).
-include("struct.hrl").
-export([start/0, stop/0, connect/2]).
-export([preMonitor/0, senderFun/1, preListener/0, deliverFun/1, generate/2]).

-define(INIT_NODES, 4).
-define(CREATE_MSG(Counter, Msg), #mcast{mid={node(), Counter + 1}, msg=Msg}).

%% Beginning of aux section
% Returns true if given name corresponds to a node of bcast
isNode(NodeName) ->
    0 < string:str(atom_to_list(NodeName), "atnode").

% Returns a list of valid bcastnode names
getNodes() ->
    lists:filter(fun(Node) -> isNode(Node) end, nodes()).

% Returns a list of nodes other than bcastnodes
getOthers() ->
    lists:filter(fun(Node) -> not isNode(Node) end, nodes()).

% Connects current bcastnode to other bcastnodes available with matching prefix (given as atom)
connect(0, _Prefix) -> done;
connect(N, Prefix) ->
    net_kernel:connect_node(list_to_atom(lists:concat([atnode,N,Prefix]))),
    connect(N - 1, Prefix).

% Disconnects current nodes
disconnect() ->
    lists:foreach(fun (Node) -> erlang:disconnect_node(Node) end, nodes()).

% Debugging function used to simulate random message behaviour
generate(_TO, 0) -> finished;
generate(TO, N) ->
    timer:sleep(TO),
    sender ! #send{msg = TO},
    generate(round(rand:uniform()*10000), N - 1).

%% Beginning of terminal functions
% Starts monitor process in current node
start() ->
    register(monitor, spawn(?MODULE, preMonitor,[])).

% Stops the processes in current node by signaling monitor to start exit protocol
stop() ->
    disconnect(),
    monitor ! {rip}.

%% Beginning of monitor functions
% Spawns necessary processes, handles closing protocol in case of process failure
preMonitor() ->
    process_flag(trap_exit, true),
    register(sender, spawn_link(?MODULE, senderFun,[0])),
    register(deliver, spawn_link(?MODULE, deliverFun,[dict:new()])),
    register(listener, spawn_link(?MODULE, preListener,[])),
    monitorFun().
monitorFun() ->
    receive
        {'EXIT', _Pid, _Ra} ->
            List = lists:delete(undefined, [whereis(sender),whereis(deliver),whereis(listener)]),
            lists:foreach(fun(X) -> X ! {rip} end, List),
            erlang:halt();
        {rip} ->
            List = lists:delete(undefined, [whereis(sender),whereis(deliver),whereis(listener)]),
            lists:foreach(fun(X) -> X ! {rip} end, List),
            exit(normal)
    end.

%% Beginning of sender functions
% Sends a message to listener process of all given nodes
sendMsg(Msg, Nodes) ->
    lists:foreach(fun (Node) -> {listener, Node} ! Msg end, Nodes).

% Sender process: receives messages from outside the bcast net and sends them to all connected bcastnodes
% creates the MID(message identifier, composed of current node and counter) for given message
senderFun(Counter) ->
    receive
        M when is_record(M, send) ->
            Msg = ?CREATE_MSG(Counter, M#send.msg),
            listener ! Msg,
            sendMsg(Msg, getNodes()),
            senderFun(Counter + 1);
        {rip} -> 
            exit(normal);
        _ ->
            io:format("Invalid msg ~n"),
            senderFun(Counter)
    end.

%% Beginning of deliver functions
% Auxiliary function to determine proposer of universal stamp
getProp(0, New, Old) -> max(New, Old);
getProp(Rest, New, _Old) when Rest > 0 -> New;
getProp(_Rest, _New, Old) -> Old.

% Deliver process: handles the responses from bcastnodes during a stamp selection
% Compares stamp values and proposers to determine the highest stamp, finally sends said stamp
% as definitive(Universal) to all other bcastnodes. Can handle multiple stamp selections at the same time.
deliverFun(Dict) ->
    receive
        {rip} ->
            exit(normal);
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
% Auxiliary function to use as preposition on sortedInsertion
propBigger({CNum, CProp, _}, {Num, Prop, _}) ->
    if
        CNum == Num -> CProp > Prop;
        true -> CNum > Num
    end.

% Auxiliary function to insert items in a sorted list
sortedInsertion([], Record) -> [Record];
sortedInsertion([CMsg | Tl], Msg) ->
    case propBigger(CMsg, Msg) of
        true ->
            [Msg | [CMsg | Tl]];
        false ->
            [CMsg|sortedInsertion(Tl, Msg)]
    end.

% Listener process:
% Responds to stamp requests and sends results as well as monitoring the other bcast nodes
% In case of stamp request responds with highest local stamp and adds received message to pending dictionary
% In case of result removes result msg from pending dictionary and adds it to definitive sorted list
% In case of nodedown notifies connected external nodes (not bcastnodes) that a node is down,
% if half of the initial ammount of nodes crashes then the service is deemed possibly inconsistent
% and shut-down (locally by each node)
% If there are messages in def list and no messages in pending then broadcasts def messages to all
% connected non-bcast nodes
preListener() ->
    net_kernel:monitor_nodes(true),
    listenerFun(0, dict:new(), [], infinity).
listenerFun(Proposal, Pend, Defin, TO) ->
    receive
        {rip} ->
            exit(normal);
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
                sortedInsertion(Defin, {M#result.hprop, M#result.proposer, Msg}), NewTO);
        {nodedown, Node} ->
            case isNode(Node) of
                true ->
                    IsLessHalf = 2*(length(getNodes())+1) =< ?INIT_NODES,
                    case IsLessHalf of
                        true ->
                            %io:format("Net down~n"),
                            sendMsg({serverdown}, getOthers()),
                            exit(down);
                        false ->
                            %io:format("Node down~n"),
                            sendMsg({nodedown}, getOthers()),
                            listenerFun(Proposal, Pend, Defin, TO)
                    end;
                false ->
                    listenerFun(Proposal, Pend, Defin, TO)
            end
    after TO ->
        case Defin of
            [{_Hprop, _Prop, Msg}|Tl] ->
                %io:format("~p ~p ~p~n", [_Hprop, _Prop, Msg]),
                sendMsg({res, Msg}, getOthers()),
                listenerFun(Proposal, Pend, Tl, 0);
            [] ->
                listenerFun(Proposal, Pend, [], infinity)
        end
    end.
