import ply.lex as lex
import ply.yacc as yacc
import sys

variables = {}

def interpret(p):
    global env
    if type(p) == tuple:
        if p[0] == '+':
            return interpret(p[1]) + interpret(p[2])
        elif p[0] == '-':
            return interpret(p[1]) - interpret(p[2])
        elif p[0] == '*':
            return interpret(p[1]) * interpret(p[2])
        elif p[0] == '/':
            return interpret(p[1]) / interpret(p[2])
        elif p[0] == '=': # Assignment of variables
            variables[p[1]] = interpret(p[2]) # ii~a = 2 is the same as (=, ii~a, 2) where p[1] is ii~a, p[2] is 2
            #print(variables)
        elif p[0] == 'var':
            return variables[p[1]]
    else:
        return p

reserved = {
	'if' : 'IF',
	'else' : 'ELSE',
	'while' : 'WHILE',
	'true' : 'TRUE',
	'false' : 'FALSE',
	'intvar' : 'INTVAR',
	'floatvar' : 'FLOATVAR',
	'input' : 'INPUT',
	'print' : 'PRINT'
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
	'COMMENT'
] + list(reserved.values())

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),

)

# Regular expression rules for simple tokens
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'

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
t_INPUT = r'in~'
t_PRINT = r'print~'
t_ignore_COMMENT = r'\#.*'

def t_TRUE(t):
	r'true'
	t.value = True
	return t

def t_FALSE(t):
	r'false'
	t.value = False
	return t

# A regular expression rule with some action code
def t_INT(t):
	r'\d+'
	t.value = int(t.value)    
	return t

def t_FLOAT(t):
	r'\d*\.\d+'
	t.value = float(t.value)
	return t

def t_STRING(t):
	r'\~.*\~'
	t.value = t.value.lstrip('~').rstrip('~')
	return t

# Variables
def t_INTVAR(t):
	r'[i]{2}\~[A-Za-z]+[A-Za-z]*'
	return t

def t_FLOATVAR(t):
	r'[f]{2}\~[A-Za-z]+[A-Za-z]*'
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

def p_arithmetic(p):
    '''
    arithmetic  : expression
          | var_assign
          | empty
    '''

    print(interpret(p[1])) 

def p_var_assign(p):
    '''
    var_assign : INTVAR EQUAL expression
    '''
    p[0] = ('=',p[1],p[3])

def p_expression(p):
	'''
	expression : expression TIMES expression
			   | expression DIVIDE expression
			   | expression PLUS expression
			   | expression MINUS expression
	'''
	p[0] = (p[2],p[1],p[3])

def p_expression_int_float(p):
	'''
	expression : INT 
			   | FLOAT
	'''
	p[0] = p[1]

def p_expression_var(p):
    '''
    expression : INTVAR
    
    '''
    p[0] = ('var',p[1])

def p_empty(p):
	'''
	empty :
	'''
	p[0] = None

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")

# try:
# 	filename = input("Enter input file:")
# 	i = open(filename, "r")
# 	if filename[-3:] != ".md":
# 		exit()

# except:
# 	print(">> Invalid file.")
# 	exit()






#data = i.read()
#lexer.input(data)
parser = yacc.yacc()


while True:
	try:
		data = input('')
	except EOFError:
		break
	parser.parse(data)

lexer.input(data)	
# Tokenize
# while True:
#     tok = lexer.token()
#     if not tok: 
#         break      # No more input

    #print(tok)
#parser.parse(data)
