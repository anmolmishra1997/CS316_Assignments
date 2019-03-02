#!/usr/bin/python3

import sys
import ply.lex as lex
import ply.yacc as yacc

## ** ##
# Here are "global" things that are used throughout ... classes, global variables
## ** ##

class Node:
    def __init__(self,type,children=None,leaf=None):
         self.type = type
         if children:
              self.children = children
         else:
              self.children = [ ]
         self.leaf = leaf

class Block:
    def __init__(self, statements = None, children=None, type = None, parents2 = None, parents = None, id = None, visited = None):
         self.type = type

         if statements:
              self.statements = statements
         else:
              self.statements = [ ]

         if children:
              self.children = children
         else:
              self.children = [ ]

         self.type = ""
         self.id = ""
         self.parents = 0
         self.parents2 = 0
         self.visited = 0

register_counter = 0 # for the count of register - to be used for intermediate calculations in cfg - global variable incremented by 1 whenever used
bid = 1 # for the id of block - global variable incremented by 1 whenever used

## ** ##
# Token definitions - for lexical analysis
## ** ##

tokens = (
        'STAR', 'AMPERSAND',
        'EQUALS', 'PLUS', 'MINUS', 'DIVIDE',
        'GE', 'LE', 'EQ', 'GT', 'LT', 'NE',
        'AND', 'OR', 'NOT',
        'LPAREN', 'RPAREN', 'LCURLY', 'RCURLY',
        'VOID', 'MAIN',
        'SEMICOLON', 'COMMA',
        'INT', 'FLOAT',
        'NAME', 'NUMBER',
        'WHILE', 'IF', 'ELSE',
)

t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    pass

t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'!'

t_STAR = r'\*'
t_AMPERSAND = r'&'

t_EQUALS = r'='
t_PLUS = r'\+'
t_MINUS = r'-'
t_DIVIDE = r'/'

t_GE = r'>='
t_LE = r'<='
t_EQ = r'=='
t_GT = r'>'
t_LT = r'<'
t_NE = r'!='

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LCURLY = r'{'
t_RCURLY = r'}'

t_SEMICOLON = r';'
t_COMMA = r','

def t_VOID(t):
    r'\bvoid\b'
    return t

# ## ## To be removed because main "not" to be treated specially
# def t_MAIN(t):
#     r'\bmain\b'
#     return t

def t_INT(t):
    r'\bint\b'
    return t

def t_FLOAT(t):
    r'\bfloat\b'
    return t

def t_WHILE(t):
    r'\bwhile\b'
    return t

def t_IF(t):
    r'\bif\b'
    return t

def t_ELSE(t):
    r'\belse\b'
    return t

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.value = "VAR(" + t.value + ")"
    return t

def t_NUMBER(t):
    r'\d+'
    try:
        t.value = "CONST(" + t.value + ")"
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_error(t):
    print("Illegal character %s" % t.value[0])
    t.lexer.skip(1)

## ** ##
# Helper functions - ast, cfg, ...
## ** ##

def number_cfg(begin):
    global bid
    if begin.parents == 0 and begin.id == "":
        if begin.statements != []:
            begin.id = "<bb " + str(bid) + ">"
            bid+=1
        for child in begin.children:
            child.parents -= 1
        for child in begin.children:
            number_cfg(child)
    else:
        return

def get_nextid(block):
    if block.id != "":
        return block.id
    else:
        try:
            return get_nextid(block.children[-1])
        except:
            print(block.statements, block.children)

def print_cfg(begin):
    global f_cfg

    if begin.parents2 == 0 and begin.visited == 0:
        begin.visited = 1
        if begin.statements != []:
            f_cfg.write(begin.id + '\n')
            if begin.type is 'IF' or begin.type is 'WHILE':
                begin.statements[-2] += get_nextid(begin.children[-2])
                begin.statements[-1] += get_nextid(begin.children[-1])
            else:
                begin.statements.append("goto " + get_nextid(begin.children[-1]))

            for statement in begin.statements:
                f_cfg.write(statement + '\n')
            f_cfg.write('\n')
        for child in begin.children:
            child.parents2 -= 1
        for child in begin.children:
            print_cfg(child)
    else:
        return

def ast(s, t):
    global f_open

    if isinstance(s, Node):
        if s.type is 'DECL':
            pass

        elif s.type is 'stmt_list':
            for child in s.children:
                ast(child, t)

        else:
            f_ast.write('\t'*t)
            f_ast.write(s.type + '\n')
            f_ast.write('\t'*t)
            f_ast.write("(" + '\n')

            for child in s.children[:-1]:
                ast(child, t+1)
                f_ast.write('\t'*(t+1))
                f_ast.write("," + '\n')

            ast(s.children[-1], t+1)
            f_ast.write('\t'*t)
            f_ast.write(")" + '\n')
    else:
        f_ast.write('\t'*t)
        f_ast.write(s + '\n')



def get_register():
    global register_counter
    to_return = str(register_counter)
    register_counter += 1
    return to_return

def cfg(s, block, endblock):
    if s.type is 'stmt_list':
        for child in s.children:
            if child.type is 'ASGN':
                process_asgn(child, block, 0)
            elif child.type is 'WHILE':
                conditionblock = Block()
                nextblock = Block()
                block.children.append(conditionblock)
                conditionblock.parents += 1
                conditionblock.parents2 += 1
                while_process(child, conditionblock, nextblock)
                block = nextblock
            elif child.type is 'IF':
                conditionblock = Block()
                nextblock = Block()
                block.children.append(conditionblock)
                conditionblock.parents += 1
                conditionblock.parents2 += 1
                if_process(child, conditionblock, nextblock)
                block = nextblock
    else:
        if s.type is 'ASGN':
            process_asgn(s, block, 0)
        elif s.type is 'WHILE':
            conditionblock = Block()
            nextblock = Block()
            block.children.append(conditionblock)
            conditionblock.parents += 1
            conditionblock.parents2 += 1
            while_process(s, conditionblock, nextblock)
            block = nextblock
        elif s.type is 'IF':
            conditionblock = Block()
            nextblock = Block()
            block.children.append(conditionblock)
            conditionblock.parents += 1
            conditionblock.parents2 += 1
            if_process(s, conditionblock, nextblock)
            block = nextblock
    block.children.append(endblock)
    if endblock.type != 'WHILE':
        endblock.parents += 1
        endblock.parents2 += 1

def process_asgn(s, block, k):
    if isinstance(s, Node):
        if s.type is 'ASGN':
            LHS = process_variable(s.children[0])
            RHS = process_asgn(s.children[1], block, k+1)
            result = LHS + ' = ' + RHS
            block.statements.append(result)

        if s.type is 'PLUS' or s.type is 'MINUS' or s.type is 'MUL' or s.type is 'DIV':
            RHS1 = process_asgn(s.children[0], block, k+1)
            RHS2 = process_asgn(s.children[1], block, k+1)
            LHS = "t" + get_register()
            result = LHS + ' = ' + RHS1 + ' ' + s.leaf + ' ' + RHS2
            block.statements.append(result)
            return LHS

        if s.type is 'UMINUS':
            RHS = process_asgn(s.children[0], block, k+1)
            LHS = "t" + get_register()
            result = LHS + ' = ' + s.leaf + ' ' + RHS
            block.statements.append(result)
            return LHS

        if s.type is 'DEREF' or s.type is 'ADDR':
            return process_variable(s)

    else:
        return process_core(s)


def while_process(s, block, nextblock):
    block.type = 'WHILE'
    condition_register = process_condition(s.children[0], block, 0)

    block.statements.append("if(" + condition_register + ") goto ")
    block.statements.append("else goto ")

    childblock = Block()
    cfg(s.children[1], childblock, block)

    block.children.append(childblock)
    childblock.parents += 1
    childblock.parents2 += 1
    block.children.append(nextblock)
    nextblock.parents += 1
    nextblock.parents2 += 1

def if_process(s, block, nextblock):
    block.type = 'IF'
    condition_register = process_condition(s.children[0], block, 0)

    block.statements.append("if(" + condition_register + ") goto ")
    block.statements.append("else goto ")

    if len(s.children) == 2:
        thenblock = Block()
        cfg(s.children[1], thenblock, nextblock)
        block.children.append(thenblock)
        thenblock.parents += 1
        thenblock.parents2 += 1
        block.children.append(nextblock)
        nextblock.parents += 1
        nextblock.parents2 += 1

    elif len(s.children) == 3:
        thenblock = Block()
        elseblock = Block()
        cfg(s.children[1], thenblock, nextblock)
        cfg(s.children[2], elseblock, nextblock)
        block.children.append(thenblock)
        thenblock.parents += 1
        thenblock.parents2 += 1
        block.children.append(elseblock)
        elseblock.parents += 1
        elseblock.parents2 += 1

def process_condition(s, block, k):
    if s.type is "AND" or s.type is "OR" :
        RHS1 = process_condition(s.children[0], block, k+1)
        RHS2 = process_condition(s.children[1], block, k+1)
        LHS = "t" + get_register()
        result = LHS + ' = ' + RHS1 + ' ' + s.leaf + ' ' + RHS2
        block.statements.append(result)
        return LHS

    elif s.type is 'NOT':
        RHS = process_condition(s.children[0], block, k+1)
        LHS = "t" + get_register()
        result = LHS + ' = ' + s.leaf + ' ' + RHS
        block.statements.append(result)
        return LHS

    else:
        return process_condition_unary(s, block, k)

def process_condition_unary(s, block, k):
    if isinstance(s, Node):
        if s.type is 'GT' or s.type is 'LT' or s.type is 'GE' or s.type is 'LE' or s.type is 'EQ' or s.type is 'NE':
            RHS1 = process_asgn(s.children[0], block, k+1)
            RHS2 = process_asgn(s.children[1], block, k+1)
            LHS = "t" + get_register()
            result = LHS + ' = ' + RHS1 + ' ' + s.leaf + ' ' + RHS2
            block.statements.append(result)
            return LHS
        else:
            return process_asgn(s, block, k)

def process_variable(s):
    if isinstance(s, Node):
        if s.type is 'DEREF' or s.type is 'ADDR':
            return s.leaf + process_variable(s.children[0])
    else:
        return process_core(s)

def process_core(s):
    first = s.find('(')
    last = s.find(')')
    return s[first+1:last]

## ** ##
# Parsing rules
## ** ##

precedence = (
        ('right', 'EQUALS'),
        ('left', 'OR'),
        ('left', 'AND'),
        ('left', 'EQ', 'NE'),
        ('left', 'GT', 'LT', 'GE', 'LE'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'DIVIDE', 'MULTIPLY'),
        ('right', 'UMINUS', 'NOT'),
        ('right', 'STAR', 'AMPERSAND'),
        ('left', 'BRACKET')
)

def p_baseprogram(p):
        """
        baseprogram : program
        """
        global f_cfg

        p[0] = p[1]
        ast(p[0], 0)

        begin = Block()
        end = Block()
        cfg(p[0], begin, end)

        number_cfg(begin)
        end.id = "<bb " + str(bid) + ">"
        print_cfg(begin)

        f_cfg.write(end.id + '\n')
        f_cfg.write("End" + '\n')


# Assuming declarations only in the beginning
# declaration_list can have function declarations also
def p_program(p):
        """
        program : declaration_list function_list
                | declaration_list
                | function_list
        """

def p_declaration_list(p):
        """
        declaration_list : declaration_list declaration 
                         | declaration
        """

def p_declaration(p):
        """
        declaration : declaration_fun
                    | declaration_var
        """

def p_declaration_fun(p):
        """
        declaration_fun : type_ret NAME LPAREN paramlist RPAREN SEMICOLON
                        | type_ret NAME LPAREN RPAREN SEMICOLON
        """

def p_declaration_var(p):
        """
        declaration_var : type varlist SEMICOLON
        """

def p_varlist(p):
        """
        varlist : var COMMA varlist
                | var
        """

def p_var(p):
        """
        var : STAR var
            | NAME
        """

def p_function_list(p):
        """
        function_list : function function_list
                      | function
        """

def p_function_1(p):
        """
        function : type_ret NAME LPAREN paramlist RPAREN body
        """
        p[0] = Node("FUNCDEF", [p[4], p[6]], p[2]) # keep the name in leaf

def p_function_2(p):
        """
        function : type_ret NAME LPAREN RPAREN body
        """
        p[0] = Node("FUNCDEF", [p[6]], p[2]) # keep the name in leaf

def p_type_ret(p):
        """
        type_ret : type
                 | VOID
        """

def p_type(p):
        """
        type : FLOAT
             | INT
        """

def p_paramlist(p):
        """
        paramlist : type var COMMA paramlist
                  | type var
        """
        p[0] = Node("PARAMLIST", [p[2],p[4]], p[1]) # putting p[1] makes sense - the type of the first child


def p_body(p):
        """
        body : LCURLY stmt_list RCURLY
             | LCURLY RCURLY
        """
        p[0] = p[2]

def p_stmt_list_1(p):
        """
        stmt_list : stmt stmt_list
        """
        a = [p[1]]
        for child in p[2].children:
            a.append(child)
        p[0] = Node("stmt_list", a, p[1])

def p_stmt_list_2(p):
        """
        stmt_list : stmt
        """
        p[0] = Node("stmt_list", [p[1]], p[1])

def p_stmt(p):
        """
        stmt : matched_stmt
             | unmatched_stmt
        """
        p[0] = p[1]

def p_matched_stmt(p):
        """
        matched_stmt : assign
                     | declaration_var
                     | matched_decision
                     | loop
                     | function_call
        """
        p[0] = p[1]

def p_unmatched_stmt(p):
        """
        unmatched_stmt : unmatched_decision
        """
        p[0] = p[1]



def p_assign(p):
        """
        assign : L1 EQUALS R1 SEMICOLON
               | L2 EQUALS R2 SEMICOLON
        """
        p[0] = Node("ASGN", [p[1], p[3]], p[2])

def p_L1(p):
        """
        L1 : NAME
        """
        p[0] = p[1]

def p_R1_expression(p):
        """
        R1 : R1 PLUS ALLNUM
           | R1 MINUS ALLNUM
           | R1 STAR ALLNUM %prec MULTIPLY
           | R1 DIVIDE ALLNUM
           | ALLNUM PLUS R1
           | ALLNUM MINUS R1
           | ALLNUM STAR R1 %prec MULTIPLY
           | ALLNUM DIVIDE R1
           | R1 PLUS R1
           | R1 MINUS R1
           | R1 STAR R1 %prec MULTIPLY
           | R1 DIVIDE R1
           | LPAREN R1 RPAREN %prec BRACKET
        """
        if p[2] == '+':
            p[0] = Node("PLUS", [p[1], p[3]], p[2])
        elif p[2] == '-':
            p[0] = Node("MINUS", [p[1], p[3]], p[2])
        elif p[2] == '*':
            p[0] = Node("MUL", [p[1], p[3]], p[2])
        elif p[2] == '/':
            p[0] = Node("DIV", [p[1], p[3]], p[2])
        else:
            p[0] = p[2]

def p_R1_uminus(p):
        """
        R1 : MINUS R1 %prec UMINUS
        """
        p[0] = Node("UMINUS", [p[2]], p[1])

def p_R1(p):
        """
        R1 : PName
           | NAME
           | function_call
        """
        p[0] = p[1]

# Added the ALLNUM rules
def p_ALLNUM_expression(p):
        """
        ALLNUM : ALLNUM PLUS ALLNUM
               | ALLNUM MINUS ALLNUM
               | ALLNUM STAR ALLNUM %prec MULTIPLY
               | ALLNUM DIVIDE ALLNUM
               | LPAREN ALLNUM RPAREN %prec BRACKET
        """
        if p[2] == '+':
            p[0] = Node("PLUS", [p[1], p[3]], p[2])
        elif p[2] == '-':
            p[0] = Node("MINUS", [p[1], p[3]], p[2])
        elif p[2] == '*':
            p[0] = Node("MUL", [p[1], p[3]], p[2])
        elif p[2] == '/':
            p[0] = Node("DIV", [p[1], p[3]], p[2])
        else:
            p[0] = p[2]

def p_ALLNUM(p):
        """
        ALLNUM : NUMBER
        """
        p[0] = p[1]

def p_L2(p):
        """
        L2 : STAR L2
           | STAR NAME
        """
        p[0] = Node("DEREF", [p[2]], p[1])

def p_R2_expression(p):
        """
        R2 : R2 PLUS R2
           | R2 MINUS R2
           | R2 STAR R2 %prec MULTIPLY
           | R2 DIVIDE R2
           | LPAREN R2 RPAREN %prec BRACKET
        """
        if p[2] == '+':
            p[0] = Node("PLUS", [p[1], p[3]], p[2])
        elif p[2] == '-':
            p[0] = Node("MINUS", [p[1], p[3]], p[2])
        elif p[2] == '*':
            p[0] = Node("MUL", [p[1], p[3]], p[2])
        elif p[2] == '/':
            p[0] = Node("DIV", [p[1], p[3]], p[2])
        else:
            p[0] = p[2]

def p_R2_uminus(p):
        """
        R2 : MINUS R2 %prec UMINUS
        """
        p[0] = Node("UMINUS", [p[2]], p[1])

def p_R2(p):
        """
        R2 : PName
           | NUMBER
           | NAME
           | function_call
        """
        p[0] = p[1]

def p_PName(p):
        """
        PName : STAR PName
              | STAR NAME
              | AMPERSAND NAME
        """
        # Only one ampersand allowed - and that too ONLY next to a variable name
        if p[1] == '*':
            p[0] = Node("DEREF", [p[2]], p[1])
        elif p[1] == '&':
            p[0] = Node("ADDR", [p[2]], p[1])

def p_function_call(p):
        """
        function_call : NAME LPAREN arglist RPAREN
                    |   NAME LPAREN RPAREN
        """
        p[0] = Node("FUNCCALL", [p[3]], p[1])

def p_arglist(p):
        """
        arglist : R2 COMMA arglist
                | R2
        """
        p[0] = Node("ARGLIST", [p[1],p[3]], p[2]) # putting p[2] makes no sense ?

def p_loop(p):
        """
        loop : WHILE LPAREN condition RPAREN body
             | WHILE LPAREN condition RPAREN stmt
             | WHILE LPAREN condition RPAREN SEMICOLON
        """
        p[0] = Node("WHILE", [p[3],p[5]], p[1]) # WHILE always has 2 children -- but the second child might be a non-Node if the THIRD rule is used

def p_condition_multiple(p):
        """
        condition : condition AND condition
                  | condition OR condition
                  | LPAREN condition RPAREN %prec BRACKET
        """
        if p[2] == '&&':
            p[0] = Node("AND", [p[1], p[3]], p[2])
        elif p[2] == '||':
            p[0] = Node("OR", [p[1], p[3]], p[2])
        else:
            p[0] = p[2]

def p_condition_unary(p):
        """
        condition : NOT condition
        """
        p[0] = Node("NOT", [p[2]], p[1])

def p_condition(p):
        """
        condition : R2 GT R2
                  | R2 LT R2
                  | R2 GE R2
                  | R2 LE R2
                  | R2 EQ R2
                  | R2 NE R2
        """
        str_type = ""
        if p[2] == '>':
            str_type = "GT"
        elif p[2] == '<':
            str_type = "LT"
        elif p[2] == '>=':
            str_type = "GE"
        elif p[2] == '<=':
            str_type = "LE"
        elif p[2] == '==':
            str_type = "EQ"
        elif p[2] == '!=':
            str_type = "NE"
        p[0] = Node(str_type, [p[1], p[3]], p[2])

def p_matched_decision(p):
        """
        matched_decision : IF LPAREN condition RPAREN matched_stmt ELSE matched_stmt
                         | IF LPAREN condition RPAREN matched_stmt ELSE body
                         | IF LPAREN condition RPAREN body ELSE matched_stmt
                         | IF LPAREN condition RPAREN body ELSE body
        """
        p[0] = Node("IF", [p[3], p[5], p[7]], p[1])

def p_unmatched_decision_1(p):
        """
        unmatched_decision : IF LPAREN condition RPAREN stmt
                           | IF LPAREN condition RPAREN body
        """
        p[0] = Node("IF", [p[3], p[5]], p[1])

def p_unmatched_decision_2(p):
        """
        unmatched_decision : IF LPAREN condition RPAREN matched_stmt ELSE unmatched_stmt
                           | IF LPAREN condition RPAREN body ELSE unmatched_stmt
        """
        p[0] = Node("IF", [p[3], p[5], p[7]], p[1])

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
    print_f_ast = "Parser_ast_" + fname + ".ast"
    f_ast = open(print_f_ast,"w")
    print_f_cfg = "Parser_cfg_" + fname + ".cfg"
    f_cfg = open(print_f_cfg,"w")
    f = open(fname, "r")
    data = f.read()

    process(data)
