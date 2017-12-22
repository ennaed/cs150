import ply.lex as lex
import ply.yacc as yacc
import sys

variables = { }

class Tuplee:
    def __init__(self, type, p1 = None, p3 = None, lineno = 0):
         self.process = type
         self.p3 = p3
         self.lineno = lineno
         if p1:
              self.p1 = p1
         else:
              self.p1 = []

def evaluate_arithmetic(symbol, x, y):
	if (symbol == "+"):
		return x + y
	elif (symbol == "-"):
		return x - y
	elif (symbol == "*"):
		return x * y
	elif (symbol == "/"):
		return x / y
	elif (symbol == "^"):
		return x ** y
	elif (symbol == "%"):
		return x % y

def evaluate_bool(symbol, x, y):
	if symbol == "||":
		return x or y
	elif symbol == "&&":
		return x and y
	elif symbol == ">":
		return x > y
	elif symbol == "<":
		return x < y
	elif symbol == "<=":
		return x <= y
	elif symbol == ">=":
		return x >= y
	elif symbol == "==":
		return x == y
	elif symbol == "!=":
		return x != y

def interpret(tuplee: Tuplee):
    global env
    if tuplee != None:

    	if len(tuplee.p1) > 0:
    		if (tuplee.process == "start" or tuplee.process == "codes" or tuplee.process == "statement"):
    			i = 0
    			while i < len(tuplee.p1):
    				interpret(tuplee.p1[i])
    				i = i+1


    	if(tuplee.process == "assign"):
    		try:
    			x = interpret(tuplee.p1[0])
    			if x != None:
    				variables[tuplee.p3[0]] = x
    				
    			else:
    				
    				print("In line # %d, Binding error. Cannot assign value to variable." % tuplee.lineno)
    		except ValueError:
    			print("In line # %d, Binding error. Cannot assign value to variable." % tuplee.lineno)

    	if(tuplee.process == "var_assign"):
    		try:
    			if len(tuplee.p3) == 2:
    				return variables[tuplee.p3[1]] * -1
    			elif len(tuplee.p3) == 1:
    				return variables[tuplee.p3[0]]
    		except LookupError:
    			print("In lint # %d, Variable is undefined." % tuplee.lineno)
    			return None

    	if(tuplee.process == "getuser_inputint"):
    		try:
    			if(len(tuplee.p1) == 0):
    				return int(input())
    			else:
    				interpret(tuplee.p1[0])
    				return int(input())
    		except TypeError:
    			print ("In line # %d, Cannot read input." % tuplee.lineno)
    			return None

    	if(tuplee.process == "getuser_inputfloat"):
    		try:
    			if(len(tuplee.p1) == 0):
    				return float(input())
    			else:
    				interpret(tuplee.p1[0])
    				return float(input())

    		except TypeError:
    			print("In line # %d, Cannot read input." % tuplee.lineno)
    			return None

    	if(tuplee.process == "getuser_inputstring"):
    		try:
    			if(len(tuplee.p1) == 0):
    				return input()
    			else:
    				interpret(tuplee.p1[0])
    				return input()

    		except TypeError:
    			print("In line # %d, Cannot read input." % tuplee.lineno)
    			return None

    	if(tuplee.process == "iterate"):
    		return interpret(tuplee.p1[0])

    	if(tuplee.process == "negative"):
    		if len(tuplee.p3) == 2:
    			return -tuplee.p3[1]
    		elif len(tuplee.p3) == 1:
    			return tuplee.p3[0]

    	if(tuplee.process == "arithmetic"):
    		try:
    			x = interpret(tuplee.p1[0])
    			y = interpret(tuplee.p1[1])
    			if x == None or y == None:
    				print("In line # %d, Cannot perform operation." % tuplee.lineno)
    				return None
    			temp = evaluate_arithmetic(tuplee.p3[0],x,y)
    			return temp

    		except ValueError:
    			print("In line # %d, Cannot perform operation" % tuplee.lineno)

    	# Array construction
    	if(tuplee.process == "array"):
    		elements = []
    		if len(tuplee.p1) == 1:
    			x = interpret(tuplee.p1[0])
    			if x == None:
    				return None
    			elements.append(x)
    			return elements
    		# temp statemen
    		elif len(tuplee.p1) == 2:
    			y = interpret(tuplee.p1[0])
    			z = interpret(tuplee.p1[1])
    			if y == None or z == None:
    				return None
    			elements.extend(y+z)
    			return elements
    		elif len(tuplee.p1) == 0:
    			return elements
    	# Conditional
    	if(tuplee.process == "if"):
    		try:
    			a = interpret(tuplee.p1[0])
    			if a:
    				interpret(tuplee.p1[1])
    		except TypeError:
    			print("In line # %d, If statement error." % tuplee.lineno)

    	if(tuplee.process == "else"):
    		try:
    			a = interpret(tuplee.p1[0])
    			if a:
    				interpret(tuplee.p1[1])
    			else:
    				interpret(tuplee.p1[2])
    		except TypeError:
    			print("In line # %d, Elseif/ Else statement error." % tuplee.lineno)

    	# Loop
    	if(tuplee.process == "while"):
    		try:
    			a = interpret(tuplee.p1[0])
    			while a:
    				interpret(tuplee.p1[1])
    				a = interpret(tuplee.p1[0])
    		except TypeError:
    			print("In line # %d, While statement error." % tuplee.lineno)

    	# Boolean
    	if(tuplee.process == "bool"):
    		return tuplee.p3[0]

    	if(tuplee.process == "comparison"):
    		try:
    			a = interpret(tuplee.p1[0])
    			b = interpret(tuplee.p1[1])
    			if a == None or b == None:
    				print("In line # %d, Cannot perform comparison." % tuplee.lineno)
    				return None
    			temp = evaluate_bool(tuplee.p3[0], a, b)
    			return temp

    		except TypeError:
    			print("In line # %d, Cannot perform comparison." % tuplee.lineno)

    	if(tuplee.process == "get_array"):
    		try:
    			temp = variables[tuplee.p3[0]]
    			ind = interpret(tuplee.p1[0])
    			if ind != None and len(temp) > ind:
    				return (temp[ind])
    			else:
    				print("In line # %d, List index out of bounds." % tuplee.lineno)
    		except LookupError:
    			print("In line # %d, Name error." % tuplee.lineno)

    	if (tuplee.process == "array_index"):
    		try:
    			a = interpret(tuplee.p1[0])
    			b = interpret(tuplee.p1[1])
    			if a == None:
    				return None
    			else:
    				return a.index(b)
    		except ValueError:
    			return -1

    	if (tuplee.process == "array_length"):
    		try:
    			a = interpret(tuplee.p1[0])
    			if a == None:
    				return None
    			else:
    				return len(a)
    		except ValueError:
    			print("In line # %d, Array length cannot be returned." % tuplee.lineno)

    	if(tuplee.process == "array_functions"):

    		interpret(tuplee.p1[0])

    	if(tuplee.process == "append"):

    		try:
    			a = interpret(tuplee.p1[0])
    			if a != None:
    				variables[tuplee.p3[0]].append(a)
    			else:
    				print("In line # %d, Element cannot be added." % tuplee.lineno)
    		except LookupError:
    			print("In line # %d, Variable is undefined." % tuplee.lineno)

    	if(tuplee.process == "change"):
    		try:
    			a = interpret(tuplee.p1[0])
    			b = interpret(tuplee.p1[1])

    			if a != None and b != None:
    				variables[tuplee.p3[0]][a] = b
    				#print(variables[tuplee.p3[0]][a])
    			else:
    				print("In line # %d, Element cannot be changed." % tuplee.lineno)
    		except LookupError:
    			
    			print("In line # %d, Element cannot be changed. " % tuplee.lineno)

    	if(tuplee.process == "display"):
    		temp = interpret(tuplee.p1[0])
    		if temp != None:
    			print(temp, end="")
    		else:
    			print("In line # %d, Value cannot be printed." % tuplee.lineno)

    	if(tuplee.process == "print"):
    		if len(tuplee.p1) == 2:
    			try:
    				a = interpret(tuplee.p1[0])
    				b = interpret(tuplee.p1[1])
    				if a == None or b == None:
    					return None
    				return str(a) + str(y)
    			except TypeError:
    				return None
    		elif len(tuplee.p1) == 1:
    			try:
    				x = interpret(tuplee.p1[0])
    				if x == None:
    					return None
    				return str(x)
    			except TypeError:
    				return None

    	if(tuplee.process == "newline"):
    		return '\n'
 
    # if type(p) == tuple:
    #     if p[0] == '+':
    #         return interpret(p[1]) + interpret(p[2])
    #     elif p[0] == '-':
    #         return interpret(p[1]) - interpret(p[2])
    #     elif p[0] == '*':
    #         return interpret(p[1]) * interpret(p[2])
    #     elif p[0] == '/':
    #         return interpret(p[1]) / interpret(p[2])
    #     elif p[0] == '=': # Assignment of variables
    #         variables[p[1]] = interpret(p[2]) # ii~a = 2 is the same as (=, ii~a, 2) where p[1] is ii~a, p[2] is 2
    #         print(variables)
    #     elif p[0] == 'var':
    #         return variables[p[1]]
    # else:
    #     return p

reserved = {
	'if' : 'IF',
	'else' : 'ELSE',
	'while' : 'WHILE',
	'true' : 'TRUE',
	'false' : 'FALSE',
	'intvar' : 'INTVAR',
	'stringvar' : 'STRINGVAR',
	'boolean' : 'BOOLVAR',
	'floatvar' : 'FLOATVAR',
	'intarray' : 'INTARRAY',
	'floatarray' : 'FLOATARRAY',
	'stringarray' : 'STRINGARRAY',
	'input' : 'INPUT',
	'index' : 'INDEX',
	'print' : 'PRINT',
	'len' : 'LENGTH'
}


# List of token names.   This is always required
tokens = [
	'INT',
	'FLOAT',
	'STRING',
	# Arithmetic
	'PLUS',
	'MINUS',
	'TIMES',
	'DIVIDE',
	'MOD',
	'EXP',

	# Assignment
	'EQUAL',

	# Logical 
	'OR',
	'AND',
	'EQUI',
	'NEQUI',
	'LT',
	'GT',
	'LTE',
	'GTE',

	# Delimiters
	'LPAREN',
	'RPAREN',
	'LBRACKET',
	'RBRACKET',
	'LBRACE',
	'RBRACE',
	'PERIOD',
	'NEWLINE'

] + list(reserved.values())

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'EXP', 'MOD'),
    ('left', 'AND', 'OR'),
    ('left', 'EQUI', 'NEQUI'),
    ('left', 'GT', 'LT'),
    ('left', 'GTE', 'LTE'),
)

# Regular expression rules for simple tokens
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_MOD = r'\%'
t_EXP = r'\^'

t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_PERIOD = r'\.'

t_EQUAL = r'\='

t_OR = r'[|]{2}'
t_AND = r'[&]{2}'
t_EQUI = r'[=]{2}'
t_NEQUI = r'!='
t_LT = r'<'
t_GT = r'>'
t_LTE = r'<='
t_GTE = r'>='

t_IF = r'if~'
t_ELSE = r'else~'
t_WHILE = r'while~'
t_INPUT = r'input~'
t_PRINT = r'print~'
t_NEWLINE = r'[/]{2}'

def t_COMMENT(t):
    r'\#.*'
    pass

def t_TRUE(t):
	r'true'
	t.value = True
	return t

def t_FALSE(t):
	r'false'
	t.value = False
	return t

# A regular expression rule with some action code

def t_FLOAT(t):
	r'\d+\.\d+'
	t.value = float(t.value)
	return t

def t_INT(t):
	r'\d+'
	t.value = int(t.value)    
	return t

def t_STRING(t):
	r'\~.*\~'
	t.value = t.value.lstrip('~').rstrip('~')
	return t

# Variables
def t_INTVAR(t):
	r'[i]{1}\~[A-Za-z]+[A-Za-z]*'
	return t

def t_FLOATVAR(t):
	r'[f]{1}\~[A-Za-z]+[A-Za-z]*'
	return t

def t_STRINGVAR(t):
	r'[s]{1}\~[A-Za-z]+[A-Za-z]*'
	return t

def t_BOOLVAR(t):
	r'[b]{1}\~[A-Za-z]+[A-Za-z]*'
	return t

# Arrays
def t_INTARRAY(t):
	r'[i]{2}\~[A-Za-z]+[A-Za-z]*'
	return t

def t_FLOATARRAY(t):
	r'[f]{2}\~[A-Za-z]+[A-Za-z]*'
	return t

def t_STRINGARRAY(t):
	r'[s]{2}\~[A-Za-z]+[A-Za-z]*'
	return t

# Define a rule so we can track line numbers
def t_newline(t):
	r'\n+'
	t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Error handling rule
def t_error(t):
	print("Illegal character '%s'" % t.value[0])
	t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

def p_start(p):
	'''
	start : code
	'''
	p[0] = Tuplee("start", [p[1]], [], p.lexer.lineno)
	interpret(p[0])

def p_code(p):
	'''
	code : line PERIOD
		 | code line PERIOD
	'''
	if len(p) == 3:
		p[0] = Tuplee("codes", [p[1]], [])
	else:
		p[0] = Tuplee("codes", [p[1], p[2]], [])


def p_line(p):
	'''
	line : assign 
	     | if_statement 
	     | while_loop 
	     | functions
	     | PRINT print
	'''
	if p[1] == None:
	    return
	if len(p) == 2:
	    p[0] = Tuplee("statement", [p[1]], [], p.lexer.lineno)
	else:
	    p[0] = Tuplee("statement", [p[2]], [], p.lexer.lineno)

def p_assign(p):
	'''
	assign : INTVAR EQUAL integer
		   | FLOATVAR EQUAL float
	   	   | BOOLVAR EQUAL bool
	       | STRINGVAR EQUAL string
	       | INTVAR EQUAL user_input_int
	       | FLOATVAR EQUAL user_input_float
	       | STRINGVAR EQUAL user_input_string
	       | INTARRAY EQUAL int_array
	       | FLOATARRAY EQUAL float_array
	       | STRINGARRAY EQUAL string_array
	'''
	p[0] = Tuplee("assign", [p[3]], [p[1]], p.lexer.lineno)

# Array creation

def p_int_array(p):
    '''int_array : LBRACKET iarray  RBRACKET
   	'''
    if len(p) == 2:
    	p[0] = Tuplee("iterate", [p[1]], [], p.lexer.lineno)
    elif len(p) == 4:
    	p[0] = Tuplee("iterate", [p[2]], [], p.lexer.lineno)

def p_float_array(p):
	'''float_array : LBRACKET farray  RBRACKET
	'''
	if len(p) == 2:
		p[0] = Tuplee("iterate", [p[1]], [], p.lexer.lineno)
	elif len(p) == 4:
		p[0] = Tuplee("iterate", [p[2]], [], p.lexer.lineno)

def p_string_array(p):
	'''string_array : LBRACKET sarray  RBRACKET
	'''
	if len(p) == 2:
		p[0] = Tuplee("iterate", [p[1]], [], p.lexer.lineno)
	elif len(p) == 4:
		p[0] = Tuplee("iterate", [p[2]], [], p.lexer.lineno)

def p_iarray(p):
	'''
	iarray : integer 
		   |
	'''
	if len(p) == 2:
		p[0] = Tuplee("array", [p[1]], [], p.lexer.lineno)
	else:
		p[0] = Tuplee("array", [], [], p.lexer.lineno)

def p_farray(p):
	'''
	farray : float 
		   |
	'''
	if len(p) == 2:
		p[0] = Tuplee("array", [p[1]], [], p.lexer.lineno)
	else:
		p[0] = Tuplee("array", [], [], p.lexer.lineno)

def p_sarray(p):
	'''
	sarray : string 
		   |
	'''
	if len(p) == 2:
		p[0] = Tuplee("array", [p[1]], [], p.lexer.lineno)
	else:
		p[0] = Tuplee("array", [], [], p.lexer.lineno)

# return array elements
def p_iarrayval(p):
	'''
	iarrayval : INTARRAY LBRACKET integer RBRACKET
	'''
	p[0] = Tuplee("get_array", [p[3]], [p[1]], p.lexer.lineno)

def p_farrayval(p):
	'''
	farrayval : FLOATARRAY LBRACKET integer RBRACKET
	'''
	p[0] = Tuplee("get_array", [p[3]], [p[1]], p.lexer.lineno)

def p_sarrayval(p):
	'''
	sarrayval : STRINGARRAY LBRACKET integer RBRACKET
	'''
	p[0] = Tuplee("get_array", [p[3]], [p[1]], p.lexer.lineno)

# Return arr element
def p_user_input_int(p):
	'''
	user_input_int : INPUT print 
				   | INPUT LPAREN RPAREN
	'''
	if len(p) == 3:
	    p[0] = Tuplee("getuser_inputint", [p[2]], [], p.lexer.lineno)
	else:
	    p[0] = Tuplee("getuser_inputint", [], [], p.lexer.lineno)
def p_user_input_float(p):
	'''
	user_input_float : INPUT print 
				   | INPUT LPAREN RPAREN
	'''
	if len(p) == 3:
	    p[0] = Tuplee("getuser_inputfloat", [p[2]], [], p.lexer.lineno)
	else:
	    p[0] = Tuplee("getuser_inputfloat", [], [], p.lexer.lineno)

def p_user_input_string(p):
	'''
	user_input_string : INPUT print 
				   | INPUT LPAREN RPAREN
	'''
	if len(p) == 3:
	    p[0] = Tuplee("getuser_inputstring", [p[2]], [], p.lexer.lineno)
	else:
	    p[0] = Tuplee("getuser_inputstring", [], [], p.lexer.lineno)

#ints
def p_integer_negvar(p):
	'''
	integer : INTVAR 
			| MINUS INTVAR
	'''
	if len(p) == 3:
		p[0] = Tuplee("var_assign", [], [p[1], p[2]], p.lexer.lineno)
	else:
		
		#print("WOOO")
		p[0] = Tuplee("var_assign", [], [p[1]], p.lexer.lineno)

def p_integer_negterm(p):
	'''
	integer : INT 
		   | MINUS INT
	'''
	if len(p) == 2:
		p[0] = Tuplee("negative", [], [p[1]], p.lexer.lineno)
	else:
		p[0] = Tuplee("negative", [], [p[1], p[2]], p.lexer.lineno)

def p_integer_array(p):
	'''
	integer : LPAREN integer RPAREN
			| iarrayval
	'''
	if len(p) == 2:
		p[0] = Tuplee("iterate", [p[1]], [], p.lexer.lineno)
	else:
		p[0] = Tuplee("iterate", [p[2]], [], p.lexer.lineno)

def p_integer_arithmetic(p):
	'''
	integer : integer PLUS integer
			| integer MINUS integer
			| integer TIMES integer
			| integer DIVIDE integer
			| integer EXP integer
			| integer MOD integer
	'''
	p[0] = Tuplee("arithmetic", [p[1], p[3]], p[2], p.lexer.lineno)

#array stuffs
def p_typeof_array(p):
	'''
	typeof_array : INTARRAY 
			     | FLOATARRAY 
			     | STRINGARRAY
	'''
	p[0] = Tuplee("var_assign", [], [p[1]], p.lexer.lineno)

def p_typeof_data(p):
	'''
	typeof_data : integer 
				| float 
				| string
	'''
	p[0] = Tuplee("iterate", [p[1]], [], p.lexer.lineno)

def p_return_arraylen(p):
	'''
	integer : string PERIOD LENGTH LPAREN RPAREN 
	        | typeof_array PERIOD LENGTH LPAREN RPAREN
	'''
	p[0] = Tuplee("array_length", [p[1]], [], p.lexer.lineno)

def p_return_arrayind(p):
	'''
	integer : typeof_array PERIOD INDEX LPAREN typeof_data RPAREN
	'''
	p[0] = Tuplee("array_index", [p[1], p[5]], [], p.lexer.lineno)

def p_array_function(p):
	'''
	functions : append 
	          | change
	'''

	p[0] = Tuplee("array_functions", [p[1]], [], p.lexer.lineno)

def p_append(p):
	'''
	append : INTARRAY PERIOD integer
		   | FLOATARRAY PERIOD float
		   | STRINGARRAY PERIOD string
	'''
	
	p[0] = Tuplee("append", [p[3]], [p[1]], p.lexer.lineno)

def p_change(p):
	'''
	change : INTARRAY LBRACKET integer RBRACKET EQUAL integer
		   | FLOATARRAY LBRACKET integer RBRACKET EQUAL float
		   | STRINGARRAY LBRACKET integer RBRACKET EQUAL string
	'''
	p[0] = Tuplee("change", [p[3], p[6]], [p[1]], p.lexer.lineno)

def p_float_negativevar(p):
	'''
	float : FLOATVAR 
		  | MINUS FLOATVAR
	'''
	if len(p) == 3:
		p[0] = Tuplee("var_assign", [], [p[1], p[2]], p.lexer.lineno)
	else:
		p[0] = Tuplee("var_assign", [], [p[1]], p.lexer.lineno)

def p_float_negterm(p):
	'''
	float : FLOAT
		  | MINUS FLOAT
	'''
	if len(p) == 2:
		p[0] = Tuplee("negative", [], [p[1]], p.lexer.lineno)
	else:
		p[0] = Tuplee("negative", [], [p[1], p[2]], p.lexer.lineno)

def p_float_neg(p):
	'''
	float : LPAREN float RPAREN
		  | farrayval
	'''
	if len(p) == 2:
		p[0] = Tuplee("iterate", [p[1]], [], p.lexer.lineno)
	else:
		p[0] = Tuplee("iterate", [p[2]], [], p.lexer.lineno)

def p_float_arithmetic(p):
	'''
	float : float PLUS float
	  	  | float MINUS float
	  	  | float TIMES float
	  	  | float DIVIDE float
	  	  | float EXP float
	  	  | float MOD float
	'''
	p[0] = Tuplee("arithmetic", [p[1], p[3]], p[2], p.lexer.lineno)

def p_string_vars(p):
	'''
	string : STRINGVAR
	'''
	p[0] = Tuplee("var_assign", [], [p[1]], p.lexer.lineno)

def p_string_arrayval(p):
	'''
	string : sarrayval
	'''
	p[0] = Tuplee("iterate", [p[1]], [], p.lexer.lineno)

def p_string_term(p):
	'''
	string : STRING
	'''
	p[0] = Tuplee("negative", [], [p[1]], p.lexer.lineno)

def p_bool_vars(p):
	'''
	bool : BOOLVAR
	'''
	p[0] = Tuplee("var_assign", [], [p[1]], p.lexer.lineno)

def p_bool_term(p):
	'''
	bool : LPAREN bool RPAREN 
		 | TRUE 
		 | FALSE
	'''
	if len(p) == 2:
		p[0] = Tuplee("bool", [], [p[1]], p.lexer.lineno)
	else:
		p[0] = Tuplee("iterate", [p[2]], [], p.lexer.lineno)

def p_bool_comparison(p):
	'''
	bool : integer AND integer
	 	 | integer OR integer
	 	 | integer EQUI integer
	 	 | integer NEQUI integer
	 	 | integer LT integer
	 	 | integer GT integer
	 	 | integer LTE integer
	 	 | integer GTE integer
	 	 | float AND float
	 	 | float OR float
	 	 | float EQUI float
	 	 | float NEQUI float
	 	 | float LT float
	 	 | float GT float
	 	 | float LTE float
	 	 | float GTE float
	 	 | bool AND bool
	 	 | bool OR bool
	 	 | bool LT bool
	 	 | bool GT bool
	 	 | bool LTE bool
	 	 | bool GTE bool
	 	 | bool EQUI bool
	 	 | bool NEQUI bool
	'''
	p[0] = Tuplee("comparison", [p[1], p[3]], [p[2]], p.lexer.lineno)

# Conditionals
def p_if_statement(p):
	'''
	if_statement : IF LPAREN bool RPAREN LBRACE code RBRACE
	'''
	p[0] = Tuplee("if", [p[3], p[6]], [], p.lexer.lineno)

def p_else_statement(p):
	'''
	if_statement : IF LPAREN bool RPAREN LBRACE code RBRACE ELSE if_statement
			 	 | IF LPAREN bool RPAREN LBRACE code RBRACE ELSE LBRACE code RBRACE

	'''
	if len(p) == 12:
		p[0] = Tuplee("else", [p[3],p[6],p[10]], [], p.lexer.lineno)
	elif len(p) == 10:
		p[0] = Tuplee("else", [p[3],p[6],p[9]], [], p.lexer.lineno)

#while


def p_while_loop(p):
	'''
	while_loop : WHILE LPAREN bool RPAREN LBRACE code RBRACE
	'''
	p[0] = Tuplee("while", [p[3], p[6]], [], p.lexer.lineno)

def p_print(p):
	'''
	print : LPAREN data RPAREN
	'''
	p[0] = Tuplee("display", [p[2]], [], p.lexer.lineno)

def p_data(p):
	'''
	data : integer
		 | string
	 	 | float
		 | newline
		 | typeof_array
		 | bool
	'''
	if len(p) == 4:
		p[0] = Tuplee("print", [p[1], p[3]], [], p.lexer.lineno)
	else:
		p[0] = Tuplee("print", [p[1]], [], p.lexer.lineno)

def p_newline(p):
	'''
	newline : NEWLINE	
	'''
	p[0] = Tuplee("newline", [], [], p.lexer.lineno)

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in line # %d" %p.lexer.lineno)
    #print(p.lexer.lineno)

try:
	filename = input("Enter input file:")
	i = open(filename, "r")
	if filename[-3:] != ".crv":
		exit()

except:
	print("Invalid file. Try: ")
	print("1. Check file location.")
	print("2. Extension must be [.crv].")
	exit()

# lexer.input(i.read())
# # Tokenize
# while True:
#     tok = lexer.token()
#     if not tok: 
#         break      # No more input
#     print(tok)

try:
	yacc.yacc()
	yacc.parse(i.read())
except:	
	print("Parsing error. Check syntax.")
	exit()
