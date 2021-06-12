NODOS = 1 2 3 4 5 6 7 8 9

all:
	$(foreach val,$(NODOS),gnome-terminal -- erl -sname node$(val);)

clean:
	rm *.beam