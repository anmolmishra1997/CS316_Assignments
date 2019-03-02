## ** ##
# Helper functions - ASM code
## ** ##

import ast_cfg

labelid = 0
float_computation = 0

def label_asm(block):
	global labelid
	if block.parents == 0 and block.id == "":
		if block.statements != []:
			block.id = "label" + str(labelid)
			labelid += 1
		for child in block.children:
			child.parents -= 1
		for child in block.children:
			label_asm(child)
	else:
		return

def get_next_label(block):
	if block.id != "":
		return block.id
	else:
		try:
			if len(block.children) > 0:
				return get_next_label(block.children[-1])
			else:
				print("** UNHANDLED CASE **")
		except:
			print("** ERROR IN get_next_label(block) **")
			print(block.statements, block.children)

def print_asm(block, print_list):
	if block.parents2 == 0 and block.visited == 0:
		block.visited = 1
		if block.statements != []:
			print_item = (block.id + ':' + '\n')
			print_list.append( print_item )
			if block.type is 'IF' or block.type is 'WHILE':
				block.statements[-2] += get_next_label(block.children[-2])
				block.statements[-1] += get_next_label(block.children[-1])
			else:
				block.statements.append("j " + get_next_label(block.children[-1]))

			for statement in block.statements:
				print_item = ('\t' + statement + '\n')
				print_list.append( print_item )
			# print_list.append( '\n' )
		for child in block.children:
			child.parents2 -= 1
		for child in block.children:
			print_asm(child, print_list)
	else:
		return

free_regs = [1,1,1,1,1,1,1,1] # 1 denotes free, 0 denotes busy
free_regt = [1,1,1,1,1,1,1,1,1,1] # 1 denotes free, 0 denotes busy

free_float_regs = [1,1,1,1,1,1,1,1] # 1 denotes free, 0 denotes busy
free_float_regt = [1,1,1,1,1,1,1,1,1,1] # 1 denotes free, 0 denotes busy

def get_register(): # will have to maintain free registers here ...
	got_reg = False
	reg_str = ""
	for elem in range(0,8):
		if free_regs[elem] == 1:
			reg_str = "$s" + str(elem)
			free_regs[elem] = 0
			got_reg = True
			break
	if not got_reg:
		for elem in range(0,10):
			if free_regt[elem] == 1:
				reg_str = "$t" + str(elem)
				free_regt[elem] = 0
				got_reg = True
				break
	if not got_reg:
		print("MAJOR FUBAR. RAN OUT OF REGISTERS!!")
	return reg_str

def free_register(reg_string):
	if reg_string[1] == 's':
		free_regs[int(reg_string[-1])] = 1
	elif reg_string[1] == 'f':
		free_float_regs[int((int(reg_string[2:]) - 10)/2)] = 1
	else:
		free_regt[int(reg_string[-1])] = 1

def get_float_register(): # will have to maintain free registers here ...
	got_reg = False
	reg_str = ""
	for elem in range(0,8):
		if free_float_regs[2 * elem] == 1:
			reg_str = "$f" + str(10 + 2 * elem)
			free_float_regs[2 * elem] = 0
			got_reg = True
			break
	# if not got_reg:
	# 	for elem in range(0,10):
	# 		if free_regt[elem] == 1:
	# 			reg_str = "$t" + str(elem)
	# 			free_regt[elem] = 0
	# 			got_reg = True
	# 			break
	if not got_reg:
		print("MAJOR FUBAR. RAN OUT OF REGISTERS!!")
	return reg_str

# def free_float_register(reg_string):
# 	if reg_string[1] == 'f':
		
	# else:
	# 	free_regt[(int(reg_string[2:]) - 10)/2] = 1

current_symbol_table = []

def assembly_top(s, block, endblock, symboltable):
	global current_symbol_table
	current_symbol_table = symboltable

	if s.type is 'function_body':
		print("** UNEXPECTED : FUNCTION BODY CASE NEEDS TO BE HANDLED **")

	elif s.type is 'stmt_list':
		for child in s.children:
			if child.type is 'ASGN':
				process_asgn(child, block, 0)
			elif child.type is 'WHILE':
				conditionblock = ast_cfg.Block()
				nextblock = ast_cfg.Block()
				block.children.append(conditionblock)
				conditionblock.parents += 1
				conditionblock.parents2 += 1
				while_process(child, conditionblock, nextblock)
				block = nextblock
			elif child.type is 'IF':
				conditionblock = ast_cfg.Block()
				nextblock = ast_cfg.Block()
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
			conditionblock = ast_cfg.Block()
			nextblock = ast_cfg.Block()
			block.children.append(conditionblock)
			conditionblock.parents += 1
			conditionblock.parents2 += 1
			while_process(s, conditionblock, nextblock)
			block = nextblock
		elif s.type is 'IF':
			conditionblock = ast_cfg.Block()
			nextblock = ast_cfg.Block()
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
	global float_computation
	# if isinstance(s, ast_cfg.Node):
	#     print("Entered process_asgn with ", s.type, s.children)
	
	if isinstance(s, ast_cfg.Node):
		if s.params['type'] == 'float':
			float_computation = 1
		elif s.params['type'] == 'int':
			float_computation = 0

		if s.type is 'ASGN':
			RHS = process_asgn(s.children[1], block, k+1)
			LHS = process_LHS(s.children[0], block)
			if s.params['type'] == 'float' and s.params['PointerLevel'] == 0:
				result = "s.s " + RHS + ', ' + LHS
			else:
				result = "sw " + RHS + ', ' + LHS

			block.statements.append(result)
			free_register(RHS)
			# now free up the LHS - there might or might not be a register there
			# LHS can have global or $sp - in which case no need
			# find a $si or $ti
			dollar = LHS.find("$")
			print(LHS)
			if dollar != -1:
				if (dollar + 2) < len(LHS):
					if LHS[dollar+2] != 'p':
						print(LHS[dollar:dollar+3])
						free_register(LHS[dollar:dollar+3])


		if s.type.startswith('CALL'):
			funcname = s.type[5:]
			list_arg = []

			for arg in s.children:
				if isinstance(arg, ast_cfg.Node) and (arg.type is 'PLUS' or arg.type is 'MINUS' or arg.type is 'MUL' or arg.type is 'DIV' or arg.type is 'UMINUS'):
					list_arg.append( process_asgn(arg, block, 0) )
				else:
					list_arg.append( "TODO" )

			block.statements.append("# setting up activation record for called function")

			glob_sym_tab = current_symbol_table.getParent()
			# print(glob_sym_tab.__dict__)
			called_table = glob_sym_tab.table['FunctionEntries'][funcname]
			# print(called_table.__dict__)
			list_offsets = called_table.getParameterOffsets()
			# print(list_offsets)
			max_elem = max(list_offsets)
			if max_elem != list_offsets[len(list_offsets) - 1]:
				print("MAJOR FUBAR. Max element of param offsets not the last one!!!")
			list_offsets = [(x - max_elem) for x in list_offsets]

			index = 0
			for arg in s.children:
				if isinstance(arg, ast_cfg.Node) and (arg.type is 'PLUS' or arg.type is 'MINUS' or arg.type is 'MUL' or arg.type is 'DIV' or arg.type is 'UMINUS'):
					# list_arg[index] - contains the register that has to be pushed
					if arg.params['type'] == 'float' and arg.params['PointerLevel'] == 0:
						block.statements.append("s.s " + list_arg[index] +  ", " + str(list_offsets[index]) + "($sp)")
					else:
						block.statements.append("sw " + list_arg[index] +  ", " + str(list_offsets[index]) + "($sp)")	
					
					free_register(list_arg[index])
				else:
					register = process_asgn(arg, block, 0)
					if arg.params['type'] == 'float' and arg.params['PointerLevel'] == 0:
						block.statements.append("s.s " + register +  ", " + str(list_offsets[index]) + "($sp)")
					else:
						block.statements.append("sw " + register +  ", " + str(list_offsets[index]) + "($sp)")					
					
					free_register(register)
				index += 1

			stack_push_len = max_elem - 12
			block.statements.append("sub $sp, $sp, " + str(stack_push_len))
			block.statements.append("jal " + funcname + " # function call")
			block.statements.append("add $sp, $sp, " + str(stack_push_len) + " # destroying activation record of called function")
			if k == 0:
				pass
			else:
				if s.params['type'] == 'float' and s.params['PointerLevel'] == 0:
					new_reg = get_float_register()
					block.statements.append("mov.s " + new_reg + ", $v1" + " # using the return value of called function")
				else:
					new_reg = get_register()
					block.statements.append("move " + new_reg + ", $v1" + " # using the return value of called function")
				return new_reg

			#Write function caller epilogue

		if s.type is 'PLUS' or s.type is 'MINUS' or s.type is 'MUL' or s.type is 'DIV':
			RHS1 = process_asgn(s.children[0], block, k+1)
			RHS2 = process_asgn(s.children[1], block, k+1)
			LHS = ""
			if s.params['type'] == 'float' and s.params['PointerLevel'] == 0:
				LHS = get_float_register()
			else:
				LHS = get_register()
			result = ""
			if s.type is 'PLUS':
				if s.params['type'] == 'float' and s.params['PointerLevel'] == 0:
					result = "add.s " + LHS + ", " + RHS1 + ", " + RHS2
				else:
					result = "add " + LHS + ", " + RHS1 + ", " + RHS2
			elif s.type is 'MINUS':
				if s.params['type'] == 'float' and s.params['PointerLevel'] == 0:
					result = "sub.s " + LHS + ", " + RHS1 + ", " + RHS2
				else:
					result = "sub " + LHS + ", " + RHS1 + ", " + RHS2				
				
			elif s.type is 'MUL':
				if s.params['type'] == 'float' and s.params['PointerLevel'] == 0:
					result = "mul.s " + LHS + ", " + RHS1 + ", " + RHS2
				else:
					result = "mul " + LHS + ", " + RHS1 + ", " + RHS2				
				
			elif s.type is 'DIV':
				if s.params['type'] == 'float' and s.params['PointerLevel'] == 0:
					result = "div.s " + LHS + ", " + RHS1 + ", " + RHS2
				else:
					result = "div " + LHS + ", " + RHS1 + ", " + RHS2				
				
			block.statements.append(result)
			free_register(RHS1)
			free_register(RHS2)
			ano_reg = ""
			if s.params['type'] == 'float' and s.params['PointerLevel'] == 0:
				ano_reg = get_float_register()
				block.statements.append("mov.s " + ano_reg + ", " + LHS)
			else:
				ano_reg = get_register()
				block.statements.append("move " + ano_reg + ", " + LHS)
			free_register(LHS)
			return ano_reg

		if s.type is 'UMINUS':
			RHS = process_asgn(s.children[0], block, k+1)
			LHS = ""
			if s.params['type'] == 'float' and s.params['PointerLevel'] == 0:
				LHS = get_float_register()
				result = "neg.s " + LHS + ", " + RHS
			else:
				LHS = get_register()
				result = "negu " + LHS + ", " + RHS
			
			block.statements.append(result)
			free_register(RHS)
			ano_reg = ""
			if s.params['type'] == 'float' and s.params['PointerLevel'] == 0:
				ano_reg = get_float_register()
				block.statements.append("mov.s " + ano_reg + ", " + LHS)
			else:
				ano_reg = get_register()
				block.statements.append("move " + ano_reg + ", " + LHS)
			
			free_register(LHS)
			return ano_reg

		if s.type is 'DEREF' or s.type is 'ADDR' or s.type is 'primitive':
			return process_variable(s, block)

	else:
		return process_core(s, block)


def while_process(s, block, nextblock):
	global current_symbol_table
	block.type = 'WHILE'
	condition_register = process_condition(s.children[0], block, 0)

	block.statements.append("bne " + condition_register + ", $0, ")
	free_register(condition_register)
	block.statements.append("j ")

	childblock = ast_cfg.Block()
	assembly_top(s.children[1], childblock, block, current_symbol_table)

	block.children.append(childblock)
	childblock.parents += 1
	childblock.parents2 += 1
	block.children.append(nextblock)
	nextblock.parents += 1
	nextblock.parents2 += 1

def if_process(s, block, nextblock):
	global current_symbol_table
	block.type = 'IF'
	condition_register = process_condition(s.children[0], block, 0)

	block.statements.append("bne " + condition_register + ", $0, ")
	free_register(condition_register)
	block.statements.append("j ")

	if len(s.children) == 2:
		thenblock = ast_cfg.Block()
		assembly_top(s.children[1], thenblock, nextblock, current_symbol_table)
		block.children.append(thenblock)
		thenblock.parents += 1
		thenblock.parents2 += 1
		block.children.append(nextblock)
		nextblock.parents += 1
		nextblock.parents2 += 1

	elif len(s.children) == 3:
		thenblock = ast_cfg.Block()
		elseblock = ast_cfg.Block()
		assembly_top(s.children[1], thenblock, nextblock, current_symbol_table)
		assembly_top(s.children[2], elseblock, nextblock, current_symbol_table)
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
		LHS = get_register()
		result = ""
		if s.type is "AND":
			result = "and " + LHS + ", " + RHS1 + ", " + RHS2
		else:
			result = "or " + LHS + ", " + RHS1 + ", " + RHS2
		block.statements.append(result)
		free_register(RHS1)
		free_register(RHS2)
		ano_reg = get_register()
		block.statements.append("move " + ano_reg + ", " + LHS)
		free_register(LHS)
		return ano_reg

	elif s.type is 'NOT':
		RHS = process_condition(s.children[0], block, k+1)
		LHS = get_register()
		result = "xori " + LHS + ", " + RHS + ", 1"
		block.statements.append(result)
		free_register(RHS)
		ano_reg = get_register()
		block.statements.append("move " + ano_reg + ", " + LHS)
		free_register(LHS)
		return ano_reg

	else:
		return process_condition_unary(s, block, k)

def process_condition_unary(s, block, k):
	if isinstance(s, ast_cfg.Node):
		if s.type is 'GT' or s.type is 'LT' or s.type is 'GE' or s.type is 'LE' or s.type is 'EQ' or s.type is 'NE':
			RHS1 = process_asgn(s.children[0], block, k+1)
			RHS2 = process_asgn(s.children[1], block, k+1)
			LHS = get_register()
			result = ""
			if s.leaf == '>':
				result = "slt " + LHS + ", " + RHS2 + ", " + RHS1
			elif s.leaf == '<':
				result = "slt " + LHS + ", " + RHS1 + ", " + RHS2
			elif s.leaf == '>=':
				result = "sle " + LHS + ", " + RHS2 + ", " + RHS1
			elif s.leaf == '<=':
				result = "sle " + LHS + ", " + RHS1 + ", " + RHS2
			elif s.leaf == '==':
				result = "seq " + LHS + ", " + RHS1 + ", " + RHS2
			elif s.leaf == '!=':
				result = "sne " + LHS + ", " + RHS1 + ", " + RHS2
			block.statements.append(result)
			free_register(RHS1)
			free_register(RHS2)
			ano_reg = get_register()
			block.statements.append("move " + ano_reg + ", " + LHS)
			free_register(LHS)
			return ano_reg
		else:
			return process_asgn(s, block, k)

def process_variable(s, block):
	if isinstance(s, ast_cfg.Node):
		if s.type is 'DEREF' or s.type is 'ADDR':
			ptr_level_here = s.params['PointerLevel']
			while isinstance(s,ast_cfg.Node) and ( s.type is 'DEREF' or s.type is 'ADDR' ):
				s = s.children[0]
				# print(s)
				# print(isinstance(s, ast_cfg.Node))
				# print("*****")
			return process_variable_2(s, block, ptr_level_here)
		elif s.type is 'primitive':
			return process_core(s.children[0], block)
	else:
		return process_core(s, block)

def process_variable_2(s, block, ptr_level_desired):
	if isinstance(s, ast_cfg.Node):
		if s.type is 'primitive':
			s = s.children[0]
		else:
			print("BIG FUBAR. Review process_variable_2.")
	# now s contains the actual variable
	if s.startswith("CONST"):
		first = s.find('(')
		last = s.find(')')
		num = s[first+1:last]
		new_reg = ""
		decimal = s.find('.')
		if decimal != -1:
			new_reg = get_float_register()
			block.statements.append("li.s " + str(new_reg) + ", " + num)
		else:
			new_reg = get_register()
			block.statements.append("li " + str(new_reg) + ", " + num)
		return new_reg
	else:
		first = s.find('(')
		last = s.find(')')
		var_str = s[first+1:last]
		if last == -1:
			var_str = s[first+1:]
		found = 0
		for elem in sorted( current_symbol_table.table['Entries'].keys() ):
			var_entry = current_symbol_table.table['Entries'][elem]
			if var_str == var_entry.name:
				found = 1
				if ptr_level_desired > var_entry.pointer_level: # banking on the fact that it can only be 1 more than var_entry.pointer_level
					new_reg = get_register()
					block.statements.append("addi " + str(new_reg) + ", $sp, " + str(var_entry.offset) )
					# print(new_reg)
					return new_reg
				elif ptr_level_desired < var_entry.pointer_level:
					new_reg = get_register()
					block.statements.append("lw " + str(new_reg) + ", " + str(var_entry.offset) + "($sp)" )
					old_reg = new_reg

					for var in range(var_entry.pointer_level - ptr_level_desired - 1):
						new_reg = get_register()
						block.statements.append("lw " + str(new_reg) + ", 0(" + str(old_reg) + ")" )
						free_register(old_reg)
						old_reg = new_reg
					new_reg = ""

					if ptr_level_desired == 0 and var_entry.data_type == 'float':
						new_reg = get_float_register()
						block.statements.append("l.s " + str(new_reg) + ", 0(" + str(old_reg) + ")" )
					else:
						new_reg = get_register()
						block.statements.append("lw " + str(new_reg) + ", 0(" + str(old_reg) + ")" )
					
					free_register(old_reg)
					old_reg = new_reg
					# print(new_reg)
					return new_reg
				else: # ptr_level_desired == var_entry.pointer_level
					new_reg = ""
					if var_entry.data_type == 'float' and var_entry.pointer_level == 0:
						new_reg = get_float_register()
						block.statements.append("l.s " + str(new_reg) + ", " + str(var_entry.offset) + "($sp)" )
					else:
						new_reg = get_register()
						block.statements.append("lw " + str(new_reg) + ", " + str(var_entry.offset) + "($sp)" )
					# print(new_reg)
					return new_reg
		if found == 0: # this means that this is a global variable
			glob_sym_tab = current_symbol_table.getParent()
			for elem in sorted( glob_sym_tab.table['Entries'].keys() ):
				var_entry = glob_sym_tab.table['Entries'][elem]
				if var_str == var_entry.name:
					if ptr_level_desired > var_entry.pointer_level: # banking on the fact that it can only be 1 more than var_entry.pointer_level
						new_reg = get_register()
						block.statements.append("la " + str(new_reg) + ", global_" + var_entry.name )
						# print(new_reg)
						return new_reg
					elif ptr_level_desired < var_entry.pointer_level:
						new_reg = get_register()
						block.statements.append("lw " + str(new_reg) + ", global_" + var_entry.name )
						old_reg = new_reg
						for var in range(var_entry.pointer_level - ptr_level_desired):
							new_reg = get_register()
							block.statements.append("lw " + str(new_reg) + ", 0(" + str(old_reg) + ")" )
							free_register(old_reg)
							old_reg = new_reg
						# print(new_reg)
						return new_reg
					else: # ptr_level_desired == var_entry.pointer_level
						new_reg = get_register()
						block.statements.append("lw " + str(new_reg) + ", global_" + var_entry.name )
						# print(new_reg)
						return new_reg

def process_core(s, block):
	if s.startswith("CONST"):
		first = s.find('(')
		last = s.find(')')
		num = s[first+1:last]
		new_reg = ""
		decimal = s.find('.')
		if decimal != -1:
			new_reg = get_float_register()
			block.statements.append("li.s " + str(new_reg) + ", " + num)
		else:
			new_reg = get_register()
			block.statements.append("li " + str(new_reg) + ", " + num)
		return new_reg
	else:
		first = s.find('(')
		last = s.find(')')
		var_str = s[first+1:last]
		if last == -1:
			var_str = s[first+1:]
		found = 0
		for elem in sorted( current_symbol_table.table['Entries'].keys() ):
			var_entry = current_symbol_table.table['Entries'][elem]
			if var_str == var_entry.name:
				found = 1
				new_reg = ""
				if var_entry.data_type == 'float' and var_entry.pointer_level == 0:
					new_reg = get_float_register()
					block.statements.append("l.s " + str(new_reg) + ", " + str(var_entry.offset) + "($sp)" )
				else:
					new_reg = get_register()
					block.statements.append("lw " + str(new_reg) + ", " + str(var_entry.offset) + "($sp)" )
				# print(new_reg)
				return new_reg
		if found == 0:
			glob_sym_tab = current_symbol_table.getParent()
			for elem in sorted( glob_sym_tab.table['Entries'].keys() ):
				var_entry = glob_sym_tab.table['Entries'][elem]
				if var_str == var_entry.name:
					new_reg = ""
					if var_entry.data_type == 'float' and var_entry.pointer_level == 0:
						new_reg = get_float_register()
						block.statements.append("l.s " + str(new_reg) + ", global_" + var_entry.name )
					else:
						new_reg = get_register()
						block.statements.append("lw " + str(new_reg) + ", global_" + var_entry.name )
					
					# print(new_reg)
					return new_reg


def process_LHS(s, block):
	if isinstance(s, ast_cfg.Node):
		if s.type is 'DEREF' or s.type is 'ADDR':
			ptr_level_here = s.params['PointerLevel']
			while isinstance(s,ast_cfg.Node) and ( s.type is 'DEREF' or s.type is 'ADDR' ):
				s = s.children[0]
				# print(s)
				# print(isinstance(s, ast_cfg.Node))
				# print("*****")
			return process_LHS_2(s, block, ptr_level_here)
		elif s.type is 'primitive':
			return process_LHS_2(s.children[0], block, s.params['PointerLevel'])
	else:
		print("MAJOR FUBAR. Review process_LHS.")

def process_LHS_2(s, block, ptr_level_desired):
	if isinstance(s, ast_cfg.Node):
		if s.type is 'primitive':
			s = s.children[0]
		else:
			print("BIG FUBAR. Review process_LHS_2.")
	# now s contains the actual variable
	if s.startswith("CONST"):
		print("UNEXPECTED CASE. Constant in LHS.")
	else:
		first = s.find('(')
		last = s.find(')')
		var_str = s[first+1:last]
		if last == -1:
			var_str = s[first+1:]
		found = 0
		for elem in sorted( current_symbol_table.table['Entries'].keys() ):
			var_entry = current_symbol_table.table['Entries'][elem]
			if var_str == var_entry.name:
				found = 1
				# print("Desired", ptr_level_desired)
				# print("Actual", var_entry.pointer_level)
				if ptr_level_desired > var_entry.pointer_level: # banking on the fact that it can only be 1 more than var_entry.pointer_level
					print("UNEXPECTED CASE. ADDR in LHS.")
				elif ptr_level_desired < var_entry.pointer_level:
					# print("HERE!")
					new_reg = get_register()
					block.statements.append("lw " + str(new_reg) + ", " + str(var_entry.offset) + "($sp)" )
					old_reg = new_reg
					for var in range(var_entry.pointer_level - ptr_level_desired - 1):
						new_reg = get_register()
						block.statements.append("lw " + str(new_reg) + ", 0(" + str(old_reg) + ")" )
						free_register(old_reg)
						old_reg = new_reg
					return "0(" + str(new_reg) + ")"
				else: # ptr_level_desired == var_entry.pointer_level
					return str(var_entry.offset) + "($sp)"
		if found == 0: # this means that this is a global variable
			glob_sym_tab = current_symbol_table.getParent()
			for elem in sorted( glob_sym_tab.table['Entries'].keys() ):
				var_entry = glob_sym_tab.table['Entries'][elem]
				if var_str == var_entry.name:
					if ptr_level_desired > var_entry.pointer_level: # banking on the fact that it can only be 1 more than var_entry.pointer_level
						print("UNEXPECTED CASE. ADDR in LHS.")
					elif ptr_level_desired < var_entry.pointer_level:
						new_reg = get_register()
						block.statements.append("lw " + str(new_reg) + ", global_" + var_entry.name )
						old_reg = new_reg
						for var in range(var_entry.pointer_level - ptr_level_desired - 1):
							new_reg = get_register()
							block.statements.append("lw " + str(new_reg) + ", 0(" + str(old_reg) + ")" )
							free_register(old_reg)
							old_reg = new_reg
						return "0(" + str(new_reg) + ")"
					else: # ptr_level_desired == var_entry.pointer_level
						return "global_" + var_entry.name
