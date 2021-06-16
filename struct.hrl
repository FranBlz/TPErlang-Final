%% Send information msg + sender
-record(send, {msg, sender}).

%% Send initial mcast petition
%% mid = {node, local_counter}
-record(mcast, {mid, msg}).

%% Result of mcast petition
-record(result, {mid, hprop, proposer}).

%% Response for mcast petition
-record(rep, {mid, hprop, proposer}).