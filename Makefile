mini_l: miniL.lex miniL.y
	bison -d -v miniL.y
	flex miniL.lex
	g++ -g -Wall -ansi -pedantic --std=c++11 lex.yy.c miniL.tab.c -lfl -o miniL

clean:
	rm -f miniL miniL.tab.* miniL.output *~ lex.yy.c lex.yy.cc mil.out