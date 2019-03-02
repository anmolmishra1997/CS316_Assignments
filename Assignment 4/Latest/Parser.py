#!/usr/bin/python3

import sys
import ply.lex as lex
import ply.yacc as yacc


class Node:
    def __init__(self,type,children=None,leaf=None, params = None):
         self.type = type
         if children:
              self.children = children
         else:
              self.children = [ ]
         self.leaf = leaf
         self.params = {}

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
        'RETURN'
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

def t_MAIN(t):
    r'\bmain\b'
    return t

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

def t_RETURN(t):
    r'\breturn\b'
    return t

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.value = 'VAR(' + t.value + ')'
    return t

def t_NUMBER(t):
    r'\d+'
    try:
        t.value = 'CONST(' + t.value + ')'
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_error(t):
    print("Illegal character %s" % t.value[0])
    t.lexer.skip(1)

bid = 1
def number_cfg(begin, end):
    global bid
    if begin.parents == 0 and begin.id == "":
        if begin.statements != []:
            begin.id = "<bb " + str(bid) + ">"
            bid+=1
        for child in begin.children:
            child.parents -= 1
        for child in begin.children:
            number_cfg(child, end)
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

def print_cfg(begin, end):
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
            print_cfg(child, end)
    else:
        return

def ast(s, t):
    global f_open

    if isinstance(s, Node):
        if s.type is 'DECL':
            pass

        elif s.type is 'stmt_list' or s.type is 'block_list':
            for child in s.children:
                ast(child, t)

        elif s.type is 'FUNCTION':
            f_ast.write('\t'*t)
            f_ast.write(s.type + s.leaf + '\n')
            f_ast.write('\t'*t)
            f_ast.write("PARAMS " + s.params['PARAMS'] + '\n')
            f_ast.write('\t'*t)
            f_ast.write("RETURNS " + s.params['RETURNS'] + '\n')
            
            for child in s.children[:-1]:
                ast(child, t+1)
                f_ast.write('\t'*(t+1))
                f_ast.write("," + '\n')

            if len(s.children) == 0:
                return

            ast(s.children[-1], t+1)
            f_ast.write('\t'*t)
            f_ast.write(")" + '\n')


        else :
            f_ast.write('\t'*t)
            f_ast.write(s.type + '\n')
            f_ast.write('\t'*t)
            f_ast.write("(" + '\n')

            for child in s.children[:-1]:
                ast(child, t+1)
                f_ast.write('\t'*(t+1))
                f_ast.write("," + '\n')


            if len(s.children) == 0:
                return

            ast(s.children[-1], t+1)
            f_ast.write('\t'*t)
            f_ast.write(")" + '\n')
    else:
        f_ast.write('\t'*t)
        f_ast.write(s + '\n')

class symbol_table():
    def __init__(self, name, parent = None):
        self.table = {'Name' : name, 'Parent' : parent, 'Entries' : {}, 'FunctionEntries' : {}}

    def getName(self):
        return self.table['Name']

    def setName(self, name):
        self.table['Name'] = name

    def getParent(self):
        return self.table['Parent']

    def getChildFunctionTable(self, func_name):
        return self.table['FunctionEntries'][func_name]

    def add(self, key, value):
        self.table['Entries'][key] = value

    def addFunction(self, key, value):
        self.table['FunctionEntries'][key] = value

    def checkIfNotPresent(self, key):
        return key not in self.table['FunctionEntries'].keys()

    def __str__(self):
        return str(self.table['Name'])

    def __repr__(self):
        return str(self.table['Name'])

class variable_table_entry():
    def __init__(self,type, name, data_type, pointer_level, width = None):
         self.type = type
         self.name = name
         self.data_type = data_type
         self.pointer_level = pointer_level
         self.width = width

    def __str__(self):
        return str(self.name)

    def __repr__(self):
        return str(self.name)

class function_table_entry():
    def __init__(self, type, name, return_type, pointer_level, parameter_types, child_table):
        self.type = type
        self.name = name
        self.return_type = return_type
        self.pointer_level = pointer_level
        self.parameter_types = parameter_types
        self.child_table = child_table


    def __str__(self):
        return str(self.name)

    def __repr__(self):
        return str(self.name)

    def getParameterTypes(self):
        return self.parameter_types

global_symbol_table = symbol_table('global')
current_symbol_table = global_symbol_table

def check_type(var1, var2):
    return var1.params['type'] == var2.params['type']

def process_core(s):
    first = s.find('(')
    last = s.find(')')
    return s[first+1:last]


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

def p_program(p):
        """
        program : block_list
        """
        global f_cfg
        p[0] = p[1]
        ast(p[0], 0)
        # begin = Block()
        # end = Block()
        # cfg(p[0], begin, end)
        # number_cfg(begin,end)
        # end.id = "<bb " + str(bid) + ">"
        # print_cfg(begin, end)
        # f_cfg.write(end.id + '\n')
        # f_cfg.write("End" + '\n')

def p_block_list_1(p):
        'block_list : basic_block block_list'
        a= [p[1]]
        for child in p[2].children:
            a.append(child)
        p[0] = Node("block_list", a, p[1])

def p_block_list_2(p):
        'block_list : basic_block'
        p[0] = Node("block_list", [p[1]], p[1])

def p_basic_block(p):
        """
        basic_block : declaration
                    | function
                    | function_prototype
        """
        p[0] = p[1]

def p_function_1(p):
        """
        function : TYPE var seen_FM LPAREN parameter_list RPAREN function_body
                | VOID NAME seen_FM LPAREN parameter_list RPAREN function_body 
        """
        global current_symbol_table


        try:
            func_name = p[2]['Name']
        except:
            func_name = process_core(p[2])

        try:
            pointer_level = p[2]['PointerLevel']
        except:
            pointer_level = 0

        if current_symbol_table.getParent().checkIfNotPresent(func_name):
            currrent_function = function_table_entry('function', func_name, p[1], pointer_level, p[5], current_symbol_table)
            current_symbol_table = current_symbol_table.getParent()
            current_symbol_table.addFunction(func_name, currrent_function)
        else:
            print(current_symbol_table.getParent)
            prototype_table = current_symbol_table.getParent().getChildFunctionTable(func_name)
            if matchParameterTypes(prototype_table.getParameterTypes(), p[5]):
                del prototype_table
                current_symbol_table.setName(func_name)
                currrent_function = function_table_entry('function', func_name, p[1], pointer_level, p[5], current_symbol_table)
                current_symbol_table = current_symbol_table.getParent()
                current_symbol_table.addFunction(func_name, currrent_function)
            else:
                print("Different types in declaration and prototype of function " + func_name)
                sys.exit()

        p[0] = Node("FUNCTION", p[7].children, func_name)
        p[0].params['RETURNS'] = p[1]
        p[0].params['PARAMS'] = '(' + str(p[5]) + ')' 

def p_function_2(p):
        """
        function : TYPE var seen_FM LPAREN RPAREN function_body
                  | VOID NAME seen_FM LPAREN RPAREN function_body
                  | VOID MAIN seen_FM LPAREN RPAREN body
        """
        global current_symbol_table


        try:
            func_name = p[2]['Name']
        except:
            func_name = process_core(p[2])

        try:
            pointer_level = p[2]['PointerLevel']
        except:
            pointer_level = 0

        if current_symbol_table.getParent().checkIfNotPresent(func_name):
            current_symbol_table.setName(func_name)
            currrent_function = function_table_entry('function', func_name, p[1], pointer_level, [], current_symbol_table)
            current_symbol_table = current_symbol_table.getParent()
            current_symbol_table.addFunction(func_name, currrent_function)
        else:
            prototype_table = current_symbol_table.getParent.getChildFunctionTable(func_name)
            if matchParameterTypes(prototype_table.getParameterTypes(), []):
                del prototype_table
                current_symbol_table.setName(func_name)
                currrent_function = function_table_entry('function', func_name, p[1], pointer_level, [], current_symbol_table)
                current_symbol_table = current_symbol_table.getParent()
                current_symbol_table.addFunction(func_name, currrent_function)
            else:
                print("Different types in declaration and prototype of function " + func_name)
                sys.exit()

        p[0] = Node("FUNCTION", p[6].children, func_name)
        p[0].params['RETURNS'] = p[1]
        p[0].params['PARAMS'] = '(' + ')'

def matchParameterTypes(List1, List2):
    if len(List1) != len(List2):
        return False
    
    for i in range(len(List1)):
        if List1[i]!=List2[i]:
            return False

    return True

def p_seen_FM(p):
        """
        seen_FM : 
        """
        global current_symbol_table

        current_symbol_table = symbol_table('temporary', current_symbol_table)

def p_function_prototype(p):
        """
        function_prototype : TYPE var seen_FM LPAREN parameter_list RPAREN SEMICOLON
                          | TYPE var seen_FM LPAREN RPAREN SEMICOLON
                          | VOID NAME seen_FM LPAREN parameter_list RPAREN SEMICOLON
                          | VOID NAME seen_FM LPAREN RPAREN SEMICOLON
        """
        p[0] = Node("DECL")

        global current_symbol_table


        try:
            func_name = p[2]['Name']
        except:
            func_name = process_core(p[2])

        try:
            pointer_level = p[2]['PointerLevel']
        except:
            pointer_level = 0

        if current_symbol_table.getParent().checkIfNotPresent(func_name):
            currrent_function = function_table_entry('function', func_name, p[1], pointer_level, p[5], current_symbol_table)
            current_symbol_table = current_symbol_table.getParent()
            current_symbol_table.addFunction(func_name, currrent_function)
        else:
            print(current_symbol_table.getParent)
            prototype_table = current_symbol_table.getParent.getChildFunctionTable(func_name)
            if matchParameterTypes(prototype_table.getParameterTypes(), p[5]):
                del prototype_table
                current_symbol_table.setName(func_name)
                currrent_function = function_table_entry('function', func_name, p[1], pointer_level, p[5], current_symbol_table)
                current_symbol_table = current_symbol_table.getParent()
                current_symbol_table.addFunction(func_name, currrent_function)
            else:
                print("Different types in declaration and prototype of function " + func_name)
                sys.exit()

def p_parameter_list(p):
        """
        parameter_list : TYPE var COMMA parameter_list
                        | TYPE var
        """
        global current_symbol_table
        
        if len(p) > 3:
            a= [p[1]]
            for child in p[4]:
                a.append(child)
            p[0] = a
        else:
            p[0] = [p[1]]

        global current_symbol_table

        var = p[2]
        curr_var = variable_table_entry( 'var', var['Name'], p[1], var['PointerLevel'], 4)            
        if current_symbol_table.checkIfNotPresent(var['Name']):
            current_symbol_table.add(var['Name'], curr_var)
        else:
            print("Multiple declaration of variable " + var['Name'])
            sys.exit()

def p_type(p):
        """
        TYPE : INT
            | FLOAT
        """
        p[0] = p[1]

def p_function_body(p):
        """
        function_body : LCURLY stmt_list return_stmt RCURLY
                    | body
        """
        if len(p) > 2:
            p[0] = Node("function_body", [p[2], p[3]], p[1])
        else:
            p[0] = Node("function_body", [p[1]], p[1])

def p_return_stmt(p):
        """
        return_stmt : RETURN SEMICOLON
                    | RETURN R2 SEMICOLON
        """
        if len(p) > 3:
            p[0] = Node("RETURN", [p[2]], p[1])
        else:
            p[0] = Node("RETURN", [], p[1])

def p_body(p):
        """
        body : LCURLY stmt_list RCURLY
                | LCURLY RCURLY
        """
        if len(p) > 3:
            p[0] = p[2]
        else:
            p[0] = ";"

def p_stmt_list_1(p):
        'stmt_list : stmt stmt_list'
        a= [p[1]]
        for child in p[2].children:
            a.append(child)
        p[0] = Node("stmt_list", a, p[1])

def p_stmt_list_2(p):
        'stmt_list : stmt'
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
                    | declaration
                    | matched_decision
                    | matched_loop
        """
        p[0] = p[1]

def p_unmatched_stmt(p):
        """
        unmatched_stmt : unmatched_decision
                        | unmatched_loop
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
        # if not check_type(p[1], p[3]):
        #     print("Error")
        #     return

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
        'R1 : MINUS R1 %prec UMINUS'
        p[0] = Node("UMINUS", [p[2]], p[1])

def p_R1(p):
        """
        R1 : PName
            | NAME
            | function_call
        """
        p[0] = p[1]

def p_ALLNUM(p):
        """
        ALLNUM : NUMBER
                | MINUS ALLNUM %prec UMINUS
        """
        if len(p) > 2:
            p[0] = Node("UMINUS", [p[2]], p[1])
        else:
            p[0] = p[1]

def p_L2(p):
        """
        L2 : STAR NAME
            | STAR L2
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
        'R2 : MINUS R2 %prec UMINUS'
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
        if p[1] == '*':
            p[0] = Node("DEREF", [p[2]], p[1])
        elif p[1] == '&':
            p[0] = Node("ADDR", [p[2]], p[1])

def p_function_call(p):
        """
        function_call : NAME LPAREN arg_list RPAREN
                    |   NAME LPAREN RPAREN
        """
        if len(p) > 4:
            p[0] = Node("CALL " + process_core(p[1]) + "(", p[3].children, p[1])
        else:
            p[0] = Node("CALL " + process_core(p[1]) + "(", [], p[1])

def p_arg_list_1(p):
        'arg_list : R2 COMMA arg_list'
        a= [p[1]]
        for child in p[3].children:
            a.append(child)
        p[0] = Node("arg_list", a, p[1])

def p_arg_list_2(p):
        'arg_list : R2'
        p[0] = Node("arg_list", [p[1]], p[1])

def p_declaration(p):
        'declaration : TYPE varlist SEMICOLON'
        p[0] = Node("DECL")

        global current_symbol_table

        for var in p[2]['List']:
            curr_var = variable_table_entry( 'var', var['Name'], p[1], var['PointerLevel'], 4)
            
            if current_symbol_table.checkIfNotPresent(var['Name']):
                current_symbol_table.add(var['Name'], curr_var)
            else:
                print("Multiple declaration of variable " + var['Name'])
                sys.exit()

def p_varlist(p):
        """
        varlist : var COMMA varlist
                | var
        """
        if len(p) > 2:
            p[3]['List'].append(p[1])
            p[0] = p[3]
        else:
            p[0] = {'List' : [p[1]]}

def p_var(p):
        """
        var : STAR var
            | NAME
        """
        if len(p) > 2:
            p[2]['PointerLevel'] += 1
            p[0] = p[2]
        else:
            p[0] = {'Name' : process_core(p[1]), 'PointerLevel' : 0}
#
# def p_NAME(p):
#         'NAME : IDENTIFIER'
#         p[0] = Node("VAR", [], p[1])
#
# def p_NUMBER(p):
#         'NAME : CONSTANT'
#         p[0] = Node("CONST", [], p[1])

def p_matched_loop(p):
        """
        matched_loop : WHILE LPAREN condition RPAREN matched_stmt
                    | WHILE LPAREN condition RPAREN body
                    | WHILE LPAREN condition RPAREN SEMICOLON
        """
        p[0] = Node("WHILE", [p[3],p[5]], p[1])

def p_unmatched_loop(p):
        'unmatched_loop : WHILE LPAREN condition RPAREN unmatched_stmt'
        p[0] = Node("WHILE", [p[3],p[5]], p[1])

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
        if p[2] == '>':
            p[0] = Node("GT", [p[1], p[3]], p[2])
        elif p[2] == '<':
            p[0] = Node("LT", [p[1], p[3]], p[2])
        elif p[2] == '>=':
            p[0] = Node("GE", [p[1], p[3]], p[2])
        elif p[2] == '<=':
            p[0] = Node("LE", [p[1], p[3]], p[2])
        elif p[2] == '==':
            p[0] = Node("EQ", [p[1], p[3]], p[2])
        elif p[2] == '!=':
            p[0] = Node("NE", [p[1], p[3]], p[2])

def p_matched_decision(p):
        """
        matched_decision : IF LPAREN condition RPAREN matched_stmt ELSE matched_stmt
                    | IF LPAREN condition RPAREN matched_stmt ELSE body
                    | IF LPAREN condition RPAREN body ELSE matched_stmt
                    | IF LPAREN condition RPAREN body ELSE body
                    | IF LPAREN condition RPAREN matched_stmt ELSE SEMICOLON
                    | IF LPAREN condition RPAREN body ELSE SEMICOLON
                    | IF LPAREN condition RPAREN SEMICOLON ELSE matched_stmt
                    | IF LPAREN condition RPAREN SEMICOLON ELSE body
                    | IF LPAREN condition RPAREN SEMICOLON ELSE SEMICOLON
        """
        p[0] = Node("IF", [p[3], p[5], p[7]], p[1])

def p_unmatched_decision_1(p):
        """
        unmatched_decision : IF LPAREN condition RPAREN stmt
                            | IF LPAREN condition RPAREN body
                            | IF LPAREN condition RPAREN SEMICOLON
        """
        p[0] = Node("IF", [p[3], p[5]], p[1])

def p_unmatched_decision_2(p):
        """
        unmatched_decision : IF LPAREN condition RPAREN matched_stmt ELSE unmatched_stmt
                            | IF LPAREN condition RPAREN body ELSE unmatched_stmt
                            | IF LPAREN condition RPAREN SEMICOLON ELSE unmatched_stmt
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
