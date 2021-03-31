all:
	flex lex.l 
	cc lex.yy.c -ll
	cat dronebnf.txt | ./a.out