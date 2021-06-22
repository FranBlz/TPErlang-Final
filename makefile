CANT_ATNODOS = 4
CANT_LEDGER_SERV = 2
CANT_LEDGERS_CLI = 2
TERMINAL = gnome-terminal
FLAG_EXEC = --

all: compile atbroadcast ledgerserv ledgercli

compile:
	erl -make

ledgerserv:
	for i in $$(seq 1 1 $(CANT_LEDGER_SERV)); do \
		$(TERMINAL) $(FLAG_EXEC) erl -sname ledger$$i -connect_all false -s ledgerServ start & \
	done

ledgercli:
	for i in $$(seq 1 1 $(CANT_LEDGERS_CLI)); do \
		$(TERMINAL) $(FLAG_EXEC) erl -sname cliente$$i -connect_all false -s ledgerCli start & \
	done

atbroadcast:
	for i in $$(seq 1 1 $(CANT_ATNODOS)); do \
		$(TERMINAL) $(FLAG_EXEC) erl -sname atnode$$i -s bcastNode start & \
	done

clean:
	rm *.beam
