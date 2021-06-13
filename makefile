NODOS = 1 2 3 4

all:
	$(foreach val,$(NODOS),gnome-terminal -- erl -sname node$(val);)

results:
	$(foreach val,$(NODOS),gnome-terminal -- erl -sname node$(val) | tee ./res/output$(val);)

clean:
	rm *.beam
	rm ./res/*