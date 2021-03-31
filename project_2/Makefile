parser: y.tab.c lex.yy.c 
	gcc -o parser y.tab.c
	cat CS315f20_team54.test.txt | ./parser
	rm lex.yy.c
	rm y.tab.c
y.tab.c: CS315f20_team54.yacc.y
	yacc CS315f20_team54.yacc.y
lex.yy.c: CS315f20_team54.lex.l
	lex CS315f20_team54.lex.l
