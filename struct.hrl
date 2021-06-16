%% Send information msg + sender
-record(send, {msg, sender}).

%% 
-record(mcast, {mid, msg}).

%% 
-record(result, {mid, hprop, proposer}).

%% Response for Seq Num
-record(rep, {mid, hprop, proposer}).