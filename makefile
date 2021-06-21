NODOS = 1 2 3 4 5 6

all: atbroadcast ledger

ledger:
	konsole -e erl -sname ledger1 -connect_all false -s ledgerServ start &
	konsole -e erl -sname ledger2 -connect_all false -s ledgerServ start &
	konsole -e erl -sname cliente1 -connect_all false -s ledgerCli start &
	konsole -e erl -sname cliente2 -connect_all false -s ledgerCli start &

atbroadcast:
	konsole -e erl -sname node1 -make &
	konsole -e erl -sname node2 -s ledgerNode start &
	konsole -e erl -sname node3 -s ledgerNode start &
	konsole -e erl -sname node4 -s ledgerNode start &
	konsole -e erl -sname node5 -s ledgerNode start &

clean:
	rm *.beam
	rm ./res/*

allFran:
	gnome-terminal -- erl -make
	sleep 3
	$(foreach val,$(NODOS),gnome-terminal -- erl -sname node$(val) -s ledgerNode start;)

ledgerFran:
	$(foreach val,$(NODOS),gnome-terminal -- erl -sname node$(val);)
	gnome-terminal -- erl -sname ledger1 -connect_all false -s ledgerServ start
	gnome-terminal -- erl -sname ledger2 -connect_all false -s ledgerServ start
	gnome-terminal -- erl -sname cliente1 -connect_all false -s ledgerCli start
	gnome-terminal -- erl -sname cliente2 -connect_all false -s ledgerCli start

resultsFran:
	$(foreach val,$(NODOS),gnome-terminal --working-directory=/home/francisco/Desktop/TPErlang-General/TPErlang-Final/ -x bash -c "erl -sname node$(val) | tee ./res/salida$(val).txt; bash";)
