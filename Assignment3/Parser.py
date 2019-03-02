#!/usr/bin/python3

import sys
import ply.lex as lex
import ply.yacc as yacc

# REMOVED THE COUNTS FOR ASGN 2
# GOTTA GENERATE THE PARSER OUT FILE

tokens = (
        'STAR', 'AMPERSAND', 'EQUALS', 'PLUS', 'MINUS', 'DIVIDE',
        'LPAREN', 'RPAREN',
        'LCURLY', 'RCURLY',
        'VOID', 'MAIN',
        'SEMICOLON', 'COMMA',
        'INT',
        'NAME', 'NUMBER',
)

t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    pass

def t_COMMENT_single(t):
    r'//[^\n]*\n'
    t.lexer.lineno += len(t.value)
    pass

# def t_COMMENT2(t):
#     r'/\*[^]*\*\\'
#     pass

t_STAR = r'\*'
t_AMPERSAND = r'&'
t_EQUALS = r'='

t_PLUS = r'\+'
t_MINUS = r'-'
t_DIVIDE = r'/'

t_LPAREN = r'\('
t_RPAREN = r'\)'

t_LCURLY = r'{'
t_RCURLY = r'}'

t_SEMICOLON = r';'
t_COMMA = r','

def t_VOID(t):
    r'\bvoid\b'
    return t

def t_MAIN(t):
    r'\bmain\b'
    return t

def t_INT(t):
    r'\bint\b'
    return t

t_NAME = r'[a-zA-Z_][a-zA-Z0-9_]*'


def t_NUMBER(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_error(t):
    print("Illegal character %s" % t.value[0])
    t.lexer.skip(1)

# sift function
def sift(s):
    s = "\t" + s
    s = s.replace('\n', '\n\t')
    return s

# Parsing rules
precedence = (
        ('right', 'EQUALS'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'DIVIDE', 'MULTIPLY'),
        ('right', 'UMINUS'),
        ('right', 'STAR', 'AMPERSAND'),
)

def p_program(p):
        'program : VOID MAIN LPAREN RPAREN body'
        pass

def p_body(p):
        """
        body : LCURLY stmt_list RCURLY
                | LCURLY RCURLY
        """
        pass

def p_stmt_list(p):
        """
        stmt_list : stmt SEMICOLON stmt_list
                    | stmt SEMICOLON
        """
        pass
        

def p_stmt(p):
        """
        stmt : assign
            | declaration
        """
        global f_open
        if p[1] != None:
            f_open.write(p[1])
            f_open.write("\n\n")

# def p_assign_list(p):
#         """
#         assign_list : assign COMMA assign_list
#                     | assign
#         """
#         pass

def p_assign(p):
        """
        assign : L1 EQUALS R1_exp
                | L2 EQUALS R1_exp
                | L2 EQUALS R2_exp
        """
        p[0] = "ASGN\n" + "(\n" + sift(p[1]) + sift("\n,") + "\n" + sift(p[3]) + "\n)"

def p_L1(p):
        """
        L1 : NAME_REC
        """
        p[0] = p[1]

def p_R1_exp(p):
        """
        R1_exp : R1_exp PLUS R2_exp
                | R1_exp MINUS R2_exp
                | R1_exp STAR R2_exp %prec MULTIPLY
                | R1_exp DIVIDE R2_exp
                | R2_exp PLUS R1_exp
                | R2_exp MINUS R1_exp
                | R2_exp STAR R1_exp %prec MULTIPLY
                | R2_exp DIVIDE R1_exp
                | R1_exp PLUS R1_exp
                | R1_exp MINUS R1_exp
                | R1_exp STAR R1_exp %prec MULTIPLY
                | R1_exp DIVIDE R1_exp
        """
        if p[2] == '+':
            p[0] = "PLUS\n" + "(\n" + sift(p[1]) + sift("\n,") + "\n" + sift(p[3]) + "\n)"
        elif p[2] == '-':
            p[0] = "MINUS\n" + "(\n" + sift(p[1]) + sift("\n,") + "\n" + sift(p[3]) + "\n)"
        elif p[2] == '*':
            p[0] = "MUL\n" + "(\n" + sift(p[1]) + sift("\n,") + "\n" + sift(p[3]) + "\n)"
        elif p[2] == '/':
            p[0] = "DIV\n" + "(\n" + sift(p[1]) + sift("\n,") + "\n" + sift(p[3]) + "\n)"

def p_R1_exp_uminus(p):
        """
        R1_exp : MINUS R1_exp %prec UMINUS
        """
        p[0] = "UMINUS\n" + "(\n" + sift(p[2]) + "\n)"

def p_R1(p):
        """
        R1_exp : NAME_REC
            | STAR R1_exp
            | AMPERSAND R1_exp
        """
        if p[1] == '*':
            p[0] = "DEREF\n" + "(\n" + sift(p[2]) + "\n)"
        elif p[1] == '&':
            p[0] = "ADDR\n" + "(\n" + sift(p[2]) + "\n)"
        else:
            p[0] = p[1]

def p_L2(p):
        """
        L2 : STAR NAME_REC 
        	| STAR L2_recur
        """
        p[0] = "DEREF\n" + "(\n" + sift(p[2]) + "\n)"

def p_L2_recur(p):
        """
        L2_recur : STAR L2_recur
                | AMPERSAND L2_recur
                | STAR NAME_REC 
                | AMPERSAND NAME_REC
        """
        if p[1] == '*':
            p[0] = "DEREF\n" + "(\n" + sift(p[2]) + "\n)"
        else:
            p[0] = "ADDR\n" + "(\n" + sift(p[2]) + "\n)"

def p_R2_exp(p):
        """
        R2_exp : R2_exp PLUS R2_exp
                | R2_exp MINUS R2_exp
                | R2_exp STAR R2_exp %prec MULTIPLY
                | R2_exp DIVIDE R2_exp
        """
        if p[2] == '+':
            p[0] = "PLUS\n" + "(\n" + sift(p[1]) + sift("\n,") + "\n" + sift(p[3]) + "\n)"
        elif p[2] == '-':
            p[0] = "MINUS\n" + "(\n" + sift(p[1]) + sift("\n,") + "\n" + sift(p[3]) + "\n)"
        elif p[2] == '*':
            p[0] = "MUL\n" + "(\n" + sift(p[1]) + sift("\n,") + "\n" + sift(p[3]) + "\n)"
        elif p[2] == '/':
            p[0] = "DIV\n" + "(\n" + sift(p[1]) + sift("\n,") + "\n" + sift(p[3]) + "\n)"

def p_R2_uminus(p):
        """
        R2_exp : MINUS R2_exp %prec UMINUS
        """
        p[0] = "UMINUS\n" + "(\n" + sift(p[2]) + "\n)"

def p_R2(p):
        """
        R2_exp : NUMBER_REC
        """
        p[0] = p[1]

# def p_R2_split(p):
#         """
#         R2_split : NAME_REC
#                 | STAR R2_split
#                 | AMPERSAND R2_split
#         """
#         if p[1] == '*':
#             p[0] = "DEREF\n" + "(\n" + sift(p[2]) + "\n)"
#         elif p[1] == '&':
#             p[0] = "ADDR\n" + "(\n" + sift(p[2]) + "\n)"
#         else:
#             p[0] = p[1]

def p_NUMBER_REC(p):
        """
        NUMBER_REC : NUMBER
        """
        p[0] = "CONST(" + str(p[1]) + ")"

def p_NAME_REC(p):
        """
        NAME_REC : NAME
        """
        p[0] = "VAR(" + p[1] + ")"

def p_STARPLUS(p):
        """
        STARPLUS : STAR STARPLUS 
                | STAR
        """

def p_declaration(p):
        """
        declaration : INT varlist
        """

def p_varlist(p):
        """
        varlist : var COMMA varlist
                | var
        """
        # p[0] = [p[1:]]

def p_var(p):
        """
        var : var_ptr
            | var_id
        """
# Can merge the following two into the above ... since differentiation not necessary here

def p_var_ptr(p):
        """
        var_ptr : STARPLUS NAME
        """

def p_var_id(p):
        """
        var_id : NAME
        """

def p_error(p):
    if p:
        print("syntax error at '{0}' line no  {1}".format(p.value, p.lineno))
    else:
        print("syntax error at EOF")

def process(data):
    lex.lex()
    yacc.yacc()
    yacc.parse(data)

print_fname = "" 

if __name__ == "__main__":
    fname = sys.argv[1]
    print_fname = "Parser_ast_" + fname + ".txt"
    f_open = open(print_fname,"w")
    f = open(fname, "r")
    data = f.read()
    
    process(data)