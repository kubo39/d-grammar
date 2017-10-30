CC=cc
LDFLAGS=-lfl -lm

FLEX ?= flex
BISON ?= bison

.PHONY: all clean

all: lexer parser-lalr

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

lexer-lalr.o: lex.yy.c parser-lalr.tab.h
	$(CC) -include parse.tab.h -c -o $@ $<

parser-lalr: parser-lalr.o parser-main.o lexer-lalr.o
	$(CC) -o $@ $^ $(LDFLAGS)

parser-lalr.o: parse.tab.c
	$(CC) -c -o $@ $<

parser-lalr-main.o: parser-lalr-main.c
	$(CC) -std=c99 -c -o $@ $<

parse.tab.c parser-lalr.tab.h: parse.y
	$(BISON) $< -d -p rs -v --report=all --warnings=error=all

clean:
	rm *.o lexer lex.yy.c
