#!/usr/bin/python3

import sys
import ply.lex as lex
import ply.yacc as yacc

import ast_cfg
# from ast_cfg import * # kept in saving.py

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
    # t.value = 'VAR(' + t.value + ')'
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

## 

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
        if self.checkIfNotPresent(func_name):
            return False
        else:
            return self.table['FunctionEntries'][func_name]

    def add(self, key, value):
        if self.checkIfVariableNotPresent(key):
            self.table['Entries'][key] = value
        else:
            print("Multiple declaration of variable " + key)
            sys.exit()

    def getVariableEntry(self, key):
        if self.checkIfVariableNotPresent(key):
            return False
        else:
            return self.table['Entries'][key]

    def addFunction(self, key, value):
        self.table['FunctionEntries'][key] = value

    def checkIfNotPresent(self, key):
        return key not in self.table['FunctionEntries'].keys()

    def checkIfVariableNotPresent(self, key):
        return key not in self.table['Entries'].keys()

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

    def generatePointerString(self):
        pointer_string = ""
        for i in range(self.pointer_level):
            pointer_string += '*'
        return pointer_string

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

    def getReturnType(self):
        pointer_string = self.generatePointerString(self.pointer_level)
        return self.return_type + pointer_string

    def getFunctionName(self):
        return self.name

    def getParameterList(self):
        parameter_string = ""
        for parameter in self.parameter_types:
            pointer_string = self.generatePointerString(parameter['PointerLevel'])
            variable_string = parameter['type'] + ' ' + pointer_string + parameter['name']
            new_string = variable_string + ', ' + parameter_string
            parameter_string = new_string
        return parameter_string[:-2]

    def generatePointerString(self, pointer_number):
        pointer_string = ""
        for i in range(pointer_number):
            pointer_string += '*'
        return pointer_string

global_symbol_table = symbol_table('global')
current_symbol_table = global_symbol_table

def check_type(var1, var2):
    return var1.params['type'] == var2.params['type']

def sym():
    global f_sym
    global global_symbol_table

    f_sym.write("Procedure table :-" + '\n')
    f_sym.write("-----------------------------------------------------------------" + '\n')
    f_sym.write("Name" + "\t\t|\t" + "Return Type" + "\t|\t" + "Parameter List" + '\n')
    for function_name in global_symbol_table.table['FunctionEntries']:
        if function_name == 'main':
            continue
        function_object = global_symbol_table.getChildFunctionTable(function_name)
        f_sym.write(function_object.getFunctionName() + "\t\t|\t" + function_object.getReturnType() + "\t|\t" + function_object.getParameterList() + '\n')

    f_sym.write("-----------------------------------------------------------------" + '\n')
    f_sym.write("Variable table :- " + '\n')
    f_sym.write("-----------------------------------------------------------------" + '\n')
    f_sym.write("Name" + "\t|\t" + "Scope" + "\t\t|\t" + "Base Type" + "\t|\t" + "Derived Type" + '\n')
    f_sym.write("-----------------------------------------------------------------" + '\n')

    for variable in global_symbol_table.table['Entries']:
        variable_object = global_symbol_table.table['Entries'][variable]
        f_sym.write(variable_object.name + "\t|\t" + "global" + "\t\t|\t" + variable_object.data_type + "\t|\t" + variable_object.generatePointerString() + '\n')

    for function_name in global_symbol_table.table['FunctionEntries']:
        function_object = global_symbol_table.getChildFunctionTable(function_name).child_table.table
        for variable in function_object['Entries']:
            variable_object = function_object['Entries'][variable]
            f_sym.write(variable_object.name + "\t|\t" + function_name + "\t\t|\t" + variable_object.data_type + "\t|\t" + variable_object.generatePointerString() + '\n')

    f_sym.write("-----------------------------------------------------------------" + '\n')
    f_sym.write("-----------------------------------------------------------------" + '\n')

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

def p_program(p):
        """
        program : block_list
        """
        global f_cfg
        global f_ast


        p[0] = p[1]
        # p[0].printing(0)
        ast_cfg.ast(p[0], 0, f_ast)

        for child in p[0].children:
            if child.type is 'DECL':
                pass
            else: # child.type is 'FUNCTION'
                funcname = ''
                if child.leaf.startswith('VAR'):
                    # this means that the function is not the MAIN, and has return type void
                    funcname = child.leaf[4:-1] # removing VAR()
                else:
                    string = child.leaf
                    deref = string.count('*')
                    funcname = string[deref:]
                f_cfg.write('function ' + funcname + child.params['PARAMS'] + '\n')

                begin = ast_cfg.Block()
                end = ast_cfg.Block()

                endid_assigned = False
                
                nodenow = child.children[0]
                if isinstance(nodenow, ast_cfg.Node) and nodenow.type is not 'RETURN':
                    if funcname == 'main':
                        print("Entered with main")
                        nodenow.printing(0)
                    ast_cfg.cfg(nodenow, begin, end)
                    ast_cfg.number_cfg(begin)
                    end.id = "<bb " + str(ast_cfg.bid) + ">"
                    # print("BID ADDRESS = ", hex(id(bid)) )
                    ast_cfg.bid += 1
                    endid_assigned = True
                    if funcname == 'main':
                        begin.printing(0)

                    print_list = []
                    ast_cfg.print_cfg(begin, print_list)
                    # print("print_list = ", print_list)
                    for elem in print_list:
                        f_cfg.write(elem)

                if not endid_assigned:
                    end.id = "<bb " + str(ast_cfg.bid) + ">"
                    ast_cfg.bid += 1

                nodenow = child.children[-1]
                if isinstance(nodenow, ast_cfg.Node) and nodenow.type is 'RETURN' and len(nodenow.children) > 0:
                    # TODO
                    f_cfg.write(end.id + '\n')

                    write_str = ast_cfg.process_asgn(nodenow.children[0], end, 0)
                    for statement in end.statements:
                        f_cfg.write(statement + '\n')
                    # print(write_str)
                    # print("----")
                    f_cfg.write("return " + write_str + '\n')
                else:
                    # Return block to be added
                    f_cfg.write(end.id + '\n')
                    f_cfg.write("return" + '\n')

                f_cfg.write('\n') # gap between two functions
        sym()


def p_block_list_1(p):
        'block_list : basic_block block_list'
        a = [p[1]]
        for child in p[2].children:
            a.append(child)
        p[0] = ast_cfg.Node("block_list", a, p[1])

def p_block_list_2(p):
        'block_list : basic_block'
        p[0] = ast_cfg.Node("block_list", [p[1]], p[1])

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
            func_name = ast_cfg.process_core(p[2])

        try:
            pointer_level = p[2]['PointerLevel']
        except:
            pointer_level = 0

        current_function = ""

        if current_symbol_table.getParent().checkIfNotPresent(func_name):
            current_symbol_table.setName(func_name)
            currrent_function = function_table_entry('function', func_name, p[1], pointer_level, p[5], current_symbol_table)
            current_symbol_table = current_symbol_table.getParent()
            current_symbol_table.addFunction(func_name, currrent_function)
        else:
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

        p[0] = ast_cfg.Node("FUNCTION", p[7].children, func_name)
        p[0].params['RETURNS'] = p[1]
        # List_params = p[5]
        # indirection_level = '*'*List_params[0]['PointerLevel']
        # varList = str(List_params[0]['type']) + indirection_level + ' ' + str(List_params[0]['name'])
        # for index in range(1,len(List_params)):
        #     indirection_level = '*'*List_params[index]['PointerLevel']
        #     addthis = str(List_params[index]['type']) + indirection_level + ' ' + str(List_params[index]['name'])
        #     varList += (', ' + addthis)

        p[0].params['PARAMS'] = '(' + currrent_function.getParameterList() + ')'

def p_function_2(p):
        """
        function : TYPE var seen_FM LPAREN RPAREN function_body
                  | VOID NAME seen_FM LPAREN RPAREN function_body
                  | VOID MAIN seen_FM LPAREN RPAREN function_body
        """
        global current_symbol_table

        try:
            func_name = p[2]['Name']
        except:
            func_name = ast_cfg.process_core(p[2])

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

        p[0] = ast_cfg.Node("FUNCTION", p[6].children, func_name)
        p[0].params['RETURNS'] = p[1]
        p[0].params['PARAMS'] = '(' + ')'

def matchParameterTypes(List1, List2):
    if len(List1) != len(List2):
        return False
    
    for i in range(len(List1)):
        if List1[i]['type'] != List2[i]['type'] or List1[i]['PointerLevel'] != List2[i]['PointerLevel']:
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
        p[0] = ast_cfg.Node("DECL")

        global current_symbol_table

        try:
            func_name = p[2]['Name']
        except:
            func_name = ast_cfg.process_core(p[2])

        try:
            pointer_level = p[2]['PointerLevel']
        except:
            pointer_level = 0

        if current_symbol_table.getParent().checkIfNotPresent(func_name):
            currrent_function = function_table_entry('function', func_name, p[1], pointer_level, p[5], current_symbol_table)
            current_symbol_table = current_symbol_table.getParent()
            current_symbol_table.addFunction(func_name, currrent_function)
        else:
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
        var = p[2]

        if len(p) > 3:
            new_var = {'type' : p[1], 'PointerLevel' : var['PointerLevel'], 'name' : var['Name']}
            p[4].append(new_var)
            p[0] = p[4]
        else:
            p[0] = [{'type' : p[1], 'PointerLevel' : var['PointerLevel'], 'name' : var['Name']}]
        
        curr_var = variable_table_entry( 'var', var['Name'], p[1], var['PointerLevel'], 4)
        current_symbol_table.add(var['Name'], curr_var)           

def p_type(p):
        """
        TYPE : INT
            | FLOAT
        """
        p[0] = p[1]

def p_function_body(p):
        """
        function_body : LCURLY stmt_list return_stmt RCURLY
                      | LCURLY return_stmt RCURLY
                      | body
        """
        if len(p) == 5:
            p[0] = ast_cfg.Node("function_body", [p[2], p[3]], p[1])
        elif len(p) == 4:
            p[0] = ast_cfg.Node("function_body", [p[2]], p[1])
        else:
            p[0] = ast_cfg.Node("function_body", [p[1]], p[1])

def p_return_stmt(p):
        """
        return_stmt : RETURN SEMICOLON
                    | RETURN R2 SEMICOLON
        """
        if len(p) > 3:
            p[0] = ast_cfg.Node("RETURN", [p[2]], p[1])
        else:
            p[0] = ast_cfg.Node("RETURN", [], p[1])

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
        p[0] = ast_cfg.Node("stmt_list", a, p[1])

def p_stmt_list_2(p):
        'stmt_list : stmt'
        p[0] = ast_cfg.Node("stmt_list", [p[1]], p[1])

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
                    | function_call SEMICOLON
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
        p[0] = ast_cfg.Node("ASGN", [p[1], p[3]], p[2], check_indirection_levels(p[1].params, p[3].params))

def p_L1(p):
        """
        L1 : NAME
        """
        p[0] = getPrimitiveParameters(p[1])
        if p[0].type == 'primitive' and p[0].params['PointerLevel'] == 0:
            print("Can't access primitive variable directly")
            sys.exit()             

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
            p[0] = ast_cfg.Node("PLUS", [p[1], p[3]], p[2], check_arithmetic_validity(p[1].params, p[3].params))
        elif p[2] == '-':
            p[0] = ast_cfg.Node("MINUS", [p[1], p[3]], p[2], check_arithmetic_validity(p[1].params, p[3].params))
        elif p[2] == '*':
            p[0] = ast_cfg.Node("MUL", [p[1], p[3]], p[2], check_arithmetic_validity(p[1].params, p[3].params))
        elif p[2] == '/':
            p[0] = ast_cfg.Node("DIV", [p[1], p[3]], p[2], check_arithmetic_validity(p[1].params, p[3].params))
        else:
            p[0] = p[2]

def p_R1_uminus(p):
        'R1 : MINUS R1 %prec UMINUS'
        p[0] = ast_cfg.Node("UMINUS", [p[2]], p[1], check_unary_arithmetic_validity(p[2].params))



def p_R1(p):
        """
        R1 : PName
            | function_call
        """
        p[0] = p[1]

def p_R1_primitives(p):
        """
        R1 : NAME
        """
        p[0] = getPrimitiveParameters(p[1])
        if p[0].type == 'primitive' and p[0].params['PointerLevel'] == 0:
            print("Can't access primitive variable directly")
            sys.exit()       

def p_ALLNUM(p):
        """
        ALLNUM : NUMBER
                | MINUS ALLNUM %prec UMINUS
        """
        if len(p) > 2:
            p[0] = ast_cfg.Node("UMINUS", [p[2]], p[1], p[2].params)
        else:
            p[0] = getPrimitiveParameters(p[1])



def p_L2(p):
        """
        L2 : STAR L2
        """
        p[0] = ast_cfg.Node("DEREF", [p[2]], p[1], p[2].params)
        changePointerLevel(p[0], -1, p)

def p_L2_primitive(p):
        """
        L2 : STAR NAME
        """
        var = getPrimitiveParameters(p[2])
        p[0] = ast_cfg.Node("DEREF", [var], p[1], var.params)
        changePointerLevel(p[0], -1, p)

def p_R2_expression(p):
        """
        R2 : R2 PLUS R2
            | R2 MINUS R2
            | R2 STAR R2 %prec MULTIPLY
            | R2 DIVIDE R2
            | LPAREN R2 RPAREN %prec BRACKET
        """
        if p[2] == '+':
            p[0] = ast_cfg.Node("PLUS", [p[1], p[3]], p[2], check_arithmetic_validity(p[1].params, p[3].params))
        elif p[2] == '-':
            p[0] = ast_cfg.Node("MINUS", [p[1], p[3]], p[2], check_arithmetic_validity(p[1].params, p[3].params))
        elif p[2] == '*':
            p[0] = ast_cfg.Node("MUL", [p[1], p[3]], p[2], check_arithmetic_validity(p[1].params, p[3].params))
        elif p[2] == '/':
            p[0] = ast_cfg.Node("DIV", [p[1], p[3]], p[2], check_arithmetic_validity(p[1].params, p[3].params))
        else:
            p[0] = p[2]

def p_R2_uminus(p):
        'R2 : MINUS R2 %prec UMINUS'
        p[0] = ast_cfg.Node("UMINUS", [p[2]], p[1], check_unary_arithmetic_validity(p[2].params))

def check_arithmetic_validity(param1, param2):
    if param1['PointerLevel'] != 0 or param2['PointerLevel'] != 0:
        print("Pointer Arithmetic Not Allowed")
        sys.exit()
    elif param1['type'] != param2['type']:
        print("Arithmetic between non-matching types")
        sys.exit()
    else:
        return param1

def check_unary_arithmetic_validity(param1):
    if param1['PointerLevel'] != 0:
        print("Pointer Arithmetic Not Allowed")
        sys.exit()
    else:
        return param1

def p_R2_Complex(p):
        """
        R2 : PName
            | function_call 
        """
        p[0] = p[1]

def p_R2_Primitives(p):
        """
        R2 : NAME 
        """
        p[0] = getPrimitiveParameters(p[1])
        if p[0].type == 'primitive' and p[0].params['PointerLevel'] == 0:
            print("Can't access primitive variable directly")
            sys.exit()

def p_R2_Number(p):
        """
        R2 : NUMBER
        """
        p[0] = getPrimitiveParameters(p[1])

def getPrimitiveParameters(primitive):
    global current_symbol_table

    if primitive[0] == 'V':
        table = current_symbol_table
        primitive_name = ast_cfg.process_core(primitive)

        while table != None:
            var = table.getVariableEntry(primitive_name)
            if var:
                final_obj = ast_cfg.Node("primitive", [primitive], primitive_name, {'type' : var.data_type, 'PointerLevel' : var.pointer_level})
                return final_obj
            table = table.getParent()

        print("Variable used without declaration " + primitive_name)
        sys.exit()
    elif primitive[0] == 'C':
        table = current_symbol_table
        number = ast_cfg.process_core(primitive)

        is_floating = number.find('.')

        if is_floating != -1:
            return ast_cfg.Node("primitive", [primitive], number, {'type' : 'float', 'PointerLevel' : 0})
        else:
            return ast_cfg.Node("primitive", [primitive], number, {'type' : 'int', 'PointerLevel' : 0})

def getFunctionParameters(function, arg_list_types):
    global current_symbol_table

    table = current_symbol_table

    while table != None:
        var = table.getChildFunctionTable(function)
        if var:
            if matchParameterTypes(var.parameter_types, arg_list_types):
                return {'type' : var.return_type, 'PointerLevel' : var.pointer_level}
            else:
                print("Types in function call do not match the types in function declaration " + function)
                sys.exit()

        table = table.getParent()

    print("Variable used without declaration " + function)
    sys.exit()

def changePointerLevel(variable, change, p):
    variable.params['PointerLevel'] += change
    if variable.params['PointerLevel'] < 0:
        print("Too much indirection ")
        print(p.stack)
        sys.exit()

def p_PName(p):
        """
        PName : STAR PName
        """
        if p[1] == '*':
            p[0] = ast_cfg.Node("DEREF", [p[2]], p[1], p[2].params)
            changePointerLevel(p[0], -1, p)
        elif p[1] == '&':
            p[0] = ast_cfg.Node("ADDR", [p[2]], p[1], p[2].params)
            changePointerLevel(p[0], 1, p)

def p_PName_Primitives(p):
        """
        PName :  STAR NAME
                | AMPERSAND NAME
        """
        var = getPrimitiveParameters(p[2])
        if p[1] == '*':
            p[0] = ast_cfg.Node("DEREF", [p[2]], p[1], var.params)
            changePointerLevel(p[0], -1, p)
        elif p[1] == '&':
            p[0] = ast_cfg.Node("ADDR", [p[2]], p[1], var.params)
            changePointerLevel(p[0], 1, p)

def p_function_call(p):
        """
        function_call : NAME LPAREN arg_list RPAREN
                    |   NAME LPAREN RPAREN
        """
        function_name = ast_cfg.process_core(p[1])

        if len(p) > 4:
            arg_list_types = p[3].params['type_list']
            params = getFunctionParameters(function_name, arg_list_types)

            p[0] = ast_cfg.Node("CALL " + function_name, p[3].children, p[1], params)
        else:
            arg_list_types = []
            params = getFunctionParameters(function_name, arg_list_types)
            p[0] = ast_cfg.Node("CALL " + function_name, [], p[1], params)

def p_arg_list_1(p):
        'arg_list : R2 COMMA arg_list'
        a = [p[1]]
        for child in p[3].children:
            a.append(child)
        p[0] = ast_cfg.Node("arg_list", a, p[1])
        p[0].params['type_list'] = [p[1].params]
        p[0].params['type_list'].extend(p[3].params['type_list'])


def p_arg_list_2(p):
        'arg_list : R2'
        p[0] = ast_cfg.Node("arg_list", [p[1]], p[1])
        p[0].params['type_list'] = [p[1].params]



def p_declaration(p):
        'declaration : TYPE varlist SEMICOLON'
        p[0] = ast_cfg.Node("DECL")

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
            p[0] = {'Name' : ast_cfg.process_core(p[1]), 'PointerLevel' : 0}
#
# def p_NAME(p):
#         'NAME : IDENTIFIER'
#         p[0] = ast_cfg.Node("VAR", [], p[1])
#
# def p_NUMBER(p):
#         'NAME : CONSTANT'
#         p[0] = ast_cfg.Node("CONST", [], p[1])

def p_matched_loop(p):
        """
        matched_loop : WHILE LPAREN condition RPAREN matched_stmt
                    | WHILE LPAREN condition RPAREN body
                    | WHILE LPAREN condition RPAREN SEMICOLON
        """
        p[0] = ast_cfg.Node("WHILE", [p[3],p[5]], p[1])

def p_unmatched_loop(p):
        'unmatched_loop : WHILE LPAREN condition RPAREN unmatched_stmt'
        p[0] = ast_cfg.Node("WHILE", [p[3],p[5]], p[1])

def p_condition_multiple(p):
        """
        condition : condition AND condition
                    | condition OR condition
                    | LPAREN condition RPAREN %prec BRACKET
        """
        if p[2] == '&&':
            p[0] = ast_cfg.Node("AND", [p[1], p[3]], p[2])
        elif p[2] == '||':
            p[0] = ast_cfg.Node("OR", [p[1], p[3]], p[2])
        else:
            p[0] = p[2]

def p_condition_unary(p):
        """
        condition : NOT condition
        """
        p[0] = ast_cfg.Node("NOT", [p[2]], p[1])

def check_indirection_levels(param1, param2):
    if param1['PointerLevel'] != param2['PointerLevel']:
        print("Different Indirection Level in Statement ")
        sys.exit()
    elif param1['type'] != param2['type']:
        print("Type mismatch in statement ")
        sys.exit()
    else:
        return param1

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
            p[0] = ast_cfg.Node("GT", [p[1], p[3]], p[2], check_indirection_levels(p[1].params, p[3].params))
        elif p[2] == '<':
            p[0] = ast_cfg.Node("LT", [p[1], p[3]], p[2], check_indirection_levels(p[1].params, p[3].params))
        elif p[2] == '>=':
            p[0] = ast_cfg.Node("GE", [p[1], p[3]], p[2], check_indirection_levels(p[1].params, p[3].params))
        elif p[2] == '<=':
            p[0] = ast_cfg.Node("LE", [p[1], p[3]], p[2], check_indirection_levels(p[1].params, p[3].params))
        elif p[2] == '==':
            p[0] = ast_cfg.Node("EQ", [p[1], p[3]], p[2], check_indirection_levels(p[1].params, p[3].params))
        elif p[2] == '!=':
            p[0] = ast_cfg.Node("NE", [p[1], p[3]], p[2], check_indirection_levels(p[1].params, p[3].params))

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
        p[0] = ast_cfg.Node("IF", [p[3], p[5], p[7]], p[1])

def p_unmatched_decision_1(p):
        """
        unmatched_decision : IF LPAREN condition RPAREN stmt
                            | IF LPAREN condition RPAREN body
                            | IF LPAREN condition RPAREN SEMICOLON
        """
        p[0] = ast_cfg.Node("IF", [p[3], p[5]], p[1])

def p_unmatched_decision_2(p):
        """
        unmatched_decision : IF LPAREN condition RPAREN matched_stmt ELSE unmatched_stmt
                            | IF LPAREN condition RPAREN body ELSE unmatched_stmt
                            | IF LPAREN condition RPAREN SEMICOLON ELSE unmatched_stmt
        """
        p[0] = ast_cfg.Node("IF", [p[3], p[5], p[7]], p[1])

def p_error(p):
    if p:
        print("syntax error at '{0}' line no  {1}".format(p.value, p.lineno))
        sys.exit()
    else:
        print("syntax error at EOF")
        sys.exit()

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
    print_f_sym = "Parser_sym_" + fname + ".sym"
    f_sym = open(print_f_sym,"w")
    f = open(fname, "r")
    data = f.read()

    process(data)