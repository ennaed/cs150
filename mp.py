import ply.lex as lex


reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'true' : 'TRUE',
    'false' : 'FALSE',
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



   # Delimiters
   'LPAREN',
   'RPAREN',
] + list(reserved.values())

# Regular expression rules for simple tokens
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'

t_EQUAL = r'\='

t_IF = r'if'
t_ELSE = r'else'
t_WHILE = r'while'


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
	t.value = t.value.lstrip('$').rstrip('$')
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


try:
	filename = input("Enter input file:")
	i = open(filename, "r")
	
except:
	print(">> Invalid file.")
	exit()

data = i.read()
lexer.input(data)

# Tokenize
while True:
    tok = lexer.token()
    if not tok: 
        break      # No more input
    print(tok)
