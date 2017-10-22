CC=cc
LDFLAGS=-lfl -lm

FLEX ?= flex
BISON ?= bison

.PHONY: all clean

all: lexer

lexer: lexer_main.o lexer.o tokens.o
	$(CC) -o $@ $^ $(LDFLAGS)

lexer_main.o: lexer_main.c
	$(CC) -c -o $@ $<

tokens.o: tokens.c
	$(CC) -std=c99 -c -o $@ $<

lex.yy.c: lexer.l
	$(FLEX) $<

lexer.o: lex.yy.c
	$(CC) -include tokens.h -c -o $@ $<

clean:
	rm *.o lexer lex.yy.c
