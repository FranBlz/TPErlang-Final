-module(ledgerServ).
-include("struct.hrl").
-export([start/0, preListener/0]).

-define(INIT_NODES, 4).

% Starts necesarry processes for ledger server
start() ->
    register(listener, spawn(?MODULE, preListener,[])).

% Auxiliary functions (same as in bcastNode)
isNode(NodeName) ->
    0 < string:str(atom_to_list(NodeName), "atnode").
getNodes() ->
    lists:filter(fun(X) -> isNode(X) end, nodes()).
getOthers() ->
    lists:filter(fun(X) -> not isNode(X) end, nodes()).

% Disconnects current nodes
disconnect() ->
    lists:foreach(fun (Node) -> erlang:disconnect_node(Node) end, nodes()).

% Auxiliary function for sending messages
sendMsg(Msg, Nodes) ->
    lists:foreach(fun(X) -> {sender, X} ! #send{msg = Msg} end, Nodes).

send2Client(Msg, Nodes) ->
    lists:foreach(fun(X) -> {listener, X} ! Msg end, Nodes).

% Listener process: handles requests from client and responses from bcast net
% In the case of bcast net responses (marked by res) it is determined first if said
% response corresponds to a query made locally, for this a record of local queries made by
% each client is kept.
% In case of problems with the atomic bcast service the connected clients are notified and
% the server attempts to reconnect to the service when possible
preListener() ->
    net_kernel:monitor_nodes(true),
    listenerFun([], [], []).
listenerFun(Ledger, GetPend, AppPend) ->
    receive
        {get, Client, Counter} ->
            sendMsg({get, Client, Counter}, getNodes()),
            listenerFun(Ledger, [{Client, Counter} | GetPend], AppPend);
        {app, Client, Counter, Value} ->
            sendMsg({app, Client, Counter, Value}, getNodes()),
            listenerFun(Ledger, GetPend, [{Client, Counter} | AppPend]);
        {res, {get, Client, Counter}} ->
            MyMsg = lists:member({Client, Counter}, GetPend),
            if
                MyMsg ->
                    {listener, Client} ! {getRes, Counter, Ledger},
                    listenerFun(Ledger, lists:delete({Client, Counter}, GetPend), AppPend);
                true ->
                    listenerFun(Ledger, GetPend, AppPend)
            end;
        {res, {app, Client, Counter, Value}} ->
            MyMsg = lists:member({Client, Counter}, GetPend),
            if
                MyMsg ->
                    listenerFun(Ledger ++ [Value], GetPend, lists:delete({Client, Counter}, AppPend));
                true ->
                    listenerFun(Ledger ++ [Value], GetPend, AppPend)
            end;
        {serverdown} ->
            send2Client({rip}, getOthers()),
            disconnect();
        {nodedown} ->
            send2Client({nottrusty}, getOthers()),
            listenerFun(Ledger, GetPend, AppPend);
        {nodedown, Node} ->
            case isNode(Node) of
                true ->
                    case reconnect(?INIT_NODES, list_to_atom(string:find(atom_to_list(Node), "@"))) of
                        serverdown -> 
                            send2Client({rip}, getOthers()),
                            disconnect();
                        done ->
                            send2Client({nottrusty}, getOthers()),
                            listenerFun(Ledger, GetPend, AppPend)
                    end;
                false ->
                    listenerFun(Ledger, GetPend, AppPend)
            end     
    end.

% Auxiliary function used to reconnect the ledger server to any available bcast node
reconnect(0, _Sufix) ->
    serverdown;
reconnect(N, Sufix) ->
    case net_kernel:connect_node(list_to_atom(lists:concat([atnode,N,Sufix]))) of
        true -> 
            done;
        false ->
            reconnect(N - 1, Sufix)
    end.