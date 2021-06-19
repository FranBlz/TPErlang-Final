NODOS = 1 2 3 4 5

all:
	$(foreach val,$(NODOS),gnome-terminal -- erl -sname node$(val);)

ledger:
	$(foreach val,$(NODOS),gnome-terminal -- erl -sname node$(val);)
	gnome-terminal -- erl -sname ledger1 -connect_all false
	gnome-terminal -- erl -sname ledger2 -connect_all false
	gnome-terminal -- erl -sname cliente1 -connect_all false
	gnome-terminal -- erl -sname cliente2 -connect_all false

resultsFran:
	$(foreach val,$(NODOS),gnome-terminal --working-directory=/home/francisco/Desktop/TPErlang-General/TPErlang-Final/ -x bash -c "erl -sname node$(val) | tee ./res/salida$(val).txt; bash";)

clean:
	rm *.beam
	rm ./res/*