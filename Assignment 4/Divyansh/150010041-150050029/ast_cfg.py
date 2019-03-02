## ** ##
# Things used for AST and CFG
## ** ##

class Node:
    def __init__(self, type, children=None, leaf=None, params = None):
        self.type = type
        if children:
          self.children = children
        else:
          self.children = [ ]
        self.leaf = leaf
        if params:
            self.params = params
        else:
            self.params = {}

    def printing(self, tabs):
        # tabs are the number of tabs to be given for visual structure
        print("Level = ",tabs)
        print("Type = ", self.type)
        print("Params = ", self.params)
        for elem in self.children:
            if isinstance(elem, Node):
                elem.printing(tabs+1)
            else:
                print(elem)


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

    def printing(self, level): # call only one of print_cfg or this, because they both use the variable 'visited'
        self.visited = 1
        print("Level = ", level)
        print("ID = ", self.id)
        print("Parents = ", self.parents, " | " , self.parents2)
        print("Statements = ", self.statements)
        for child in self.children:
            if child.visited == 0:
                child.printing(level+1)

register_counter = 0 # for the count of register - to be used for intermediate calculations in cfg - global variable incremented by 1 whenever used
bid = 0 # for the id of block - global variable incremented by 1 whenever used
# bid initialisation has been changed from A3 to A4 from 1 to 0

## ** ##
# Helper functions - AST
## ** ##

def ast(s, t, filename):
    if isinstance(s, Node):
        if s.type is 'DECL':
            pass

        elif s.type is 'primitive':
            ast(s.children[0], t, filename)

        elif s.type is 'stmt_list' or s.type is 'block_list':
            for child in s.children:
                ast(child, t, filename)

        elif s.type is 'FUNCTION':
            # print("No of children = ", len(s.children))
            filename.write('\t'*t)

            funcname = ''
            ret_suffix = ''
            if s.leaf.startswith('VAR'):
                # this means that the function is not the MAIN, and has return type void
                funcname = s.leaf[4:-1] # removing VAR()
            else:
                string = s.leaf
                deref = string.count('*')
                funcname = string[deref:]
                ret_suffix = '*'*deref
            
            if funcname == 'main':
                funcname = 'Main' # to match the given format

            filename.write(s.type + ' ' + funcname + '\n')
            filename.write('\t'*t)
            filename.write("PARAMS " + s.params['PARAMS'] + '\n')
            filename.write('\t'*t)
            filename.write("RETURNS " + s.params['RETURNS'] + ret_suffix + '\n')
            
            # print("-----")
            # print(funcname)

            # for child in s.children[:]:
            #     if isinstance(child, Node):
            #         print(child.type)
            #     else:
            #         print(child)

            # print("-----")

            if len(s.children) == 0:
                pass
            elif len(s.children) == 1:
                if isinstance(s.children[0], Node) and s.children[0].type is 'RETURN':
                    ast(s.children[0], 0, filename)
                else:
                    ast(s.children[0], t+1, filename)
            else:
                for child in s.children[:-2]:
                    ast(child, t+1, filename)
                    filename.write('\t'*(t))

                if s.children[-1].type is not 'RETURN':
                    ast(s.children[-2], t+1, filename)
                    filename.write('\t'*(t))
                    ast(s.children[-1], t+1, filename)
                else:
                    ast(s.children[-2], t+1, filename)
                    # filename.write('\t'*(t+1))
                    ast(s.children[-1], 0, filename) # RETURN statement at same indentation

            filename.write('\n')

        else :
            filename.write('\t'*t)
            filename.write(s.type)
            
            if s.type.startswith('CALL'):
                filename.write("(" + '\n')
            else:
                filename.write('\n')
                filename.write('\t'*t) 
                filename.write("(" + '\n')

            if len(s.children) == 0:
                filename.write('\t'*t)
            else:
                for child in s.children[:-1]:
                    ast(child, t+1, filename)
                    filename.write('\t'*(t+1))
                    filename.write("," + '\n')
                ast(s.children[-1], t+1, filename)
                filename.write('\t'*t)

            filename.write(")" + '\n')
    else:
        filename.write('\t'*t)
        filename.write(s + '\n')

## ** ##
# Helper functions - CFG
## ** ##

def number_cfg(block):
    global bid
    if block.parents == 0 and block.id == "":
        if block.statements != []:
            block.id = "<bb " + str(bid) + ">"
            # print("bid address = ", hex(id(bid)) )
            bid += 1
            # print("bid = ", bid)
        for child in block.children:
            child.parents -= 1
        for child in block.children:
            number_cfg(child)
    else:
        return

def get_nextid(block):
    if block.id != "":
        return block.id
    else:
        try:
            if len(block.children) > 0:
                return get_nextid(block.children[-1])
            else:
                print("** UNHANDLED CASE **")
        except:
            print("** ERROR IN get_nextid(block) **")
            print(block.statements, block.children)

def print_cfg(block, print_list):
    if block.parents2 == 0 and block.visited == 0:
        block.visited = 1
        if block.statements != []:
            print_item = (block.id + '\n')
            print_list.append( print_item )
            if block.type is 'IF' or block.type is 'WHILE':
                block.statements[-2] += get_nextid(block.children[-2])
                block.statements[-1] += get_nextid(block.children[-1])
            else:
                block.statements.append("goto " + get_nextid(block.children[-1]))

            for statement in block.statements:
                print_item = (statement + '\n')
                print_list.append( print_item )
            print_list.append( '\n' )
        for child in block.children:
            child.parents2 -= 1
        for child in block.children:
            print_cfg(child, print_list)
    else:
        return

def get_register():
    global register_counter
    to_return = str(register_counter)
    register_counter += 1
    return to_return

def cfg(s, block, endblock):
    # print("Entered cfg() with ", s.type)
    
    # FOLLOWING IS HANDLED DIRECTLY IN PARSER.PY

    # if s.type is 'block_list':
    #     for child in s.children:
    #         if child.type is 'DECL':
    #             pass
    #         elif child.type is 'FUNCTION':
    #             cfg(child, block, endblock)
    #         else:
    #             print("** UNEXPECTED : UNHANDLED CASE **")

    # if s.type is 'FUNCTION':
    #     for child in s.children:
    #         cfg(child, block, endblock)

    if s.type is 'function_body':
        print("** UNEXPECTED : FUNCTION BODY CASE NEEDS TO BE HANDLED **")

    elif s.type is 'stmt_list':
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
            elif child.type.startswith('CALL'):
                process_asgn(child, block, 0)

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
        elif s.type.startswith('CALL'):
            process_asgn(s, block, 0)

    block.children.append(endblock)
    if endblock.type != 'WHILE':
        endblock.parents += 1
        endblock.parents2 += 1

def process_asgn(s, block, k):
    # if isinstance(s, Node):
    #     print("Entered process_asgn with ", s.type, s.children)
    
    if isinstance(s, Node):
        if s.type is 'ASGN':
            LHS = process_variable(s.children[0])
            RHS = process_asgn(s.children[1], block, k+1)
            result = LHS + ' = ' + RHS
            block.statements.append(result)

        if s.type.startswith('CALL'):
            list_arg = []
            for arg in s.children:
                list_arg.append( process_asgn(arg, block, 0) )
            result = s.type[5:]
            if list_arg:
                result += list_arg[0]
                for index in range( 1,len(list_arg) ):
                    result += ', ' + list_arg[index]
            result += ')'
            if k == 0:
                block.statements.append(result)
            else:
                return result

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

        if s.type is 'DEREF' or s.type is 'ADDR' or s.type is 'primitive':
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
            # print(s.leaf)
            # print(len(s.children))
            # print(s.children)
            # print("----")
            return s.leaf + process_variable(s.children[0])
        elif s.type is 'primitive':
            return process_core(s.children[0])
    else:
        return process_core(s)

def process_core(s):
    first = s.find('(')
    last = s.find(')')
    return s[first+1:last]