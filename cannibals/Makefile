all: cannibals

cannibals: main.o sstack.o
	gcc main.o sstack.o -o cannibals

main.o: main.c sstack.h
	gcc -c main.c

sstack.o: sstack.c sstack.h
	gcc -c sstack.c

.phony: debug
debug: cannibals.debug
	gdb cannibals.debug

cannibals.debug: main.c sstack.c sstack.h
	gcc -g main.c sstack.c -o cannibals.debug
