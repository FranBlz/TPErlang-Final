%% Send information msg + sender
-record(send, {msg, sender}).

%% Msg info Msg x Sender x Seq Num
-record(msg, {body, sender, sn}).

%% Proposal for Seq Num
-record(prop, {sender, sn}).

%% Response for Seq Num
-record(rep, {sender, sn}).