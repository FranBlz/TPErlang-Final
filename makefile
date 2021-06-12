NODOS = 1 2 3 4

all:
	$(foreach val,$(NODOS),gnome-terminal -- erl -sname node$(val);)

clean:
	rm *.beam