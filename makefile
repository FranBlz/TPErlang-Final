NODOS = 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20

all:
	$(foreach val,$(NODOS),gnome-terminal -- erl -sname node$(val);)

resultsFran:
	$(foreach val,$(NODOS),gnome-terminal --working-directory=/home/francisco/Desktop/TPErlang-General/TPErlang-Final/ -x bash -c "erl -sname node$(val) | tee ./res/salida$(val).txt; bash";)

clean:
	rm *.beam
	rm ./res/*