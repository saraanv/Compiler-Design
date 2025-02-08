import ply.lex as lex
import ply.yacc as yacc

# lexer part

tokens = [
    'ID', 'INT_LITERAL', 'HEX_LITERAL', 'CHAR_LITERAL', 'STRING_LITERAL',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MOD',
    'EQ', 'NEQ', 'LE', 'GE', 'LT', 'GT',
    'AND', 'OR', 'NOT',
    'LBRACE', 'RBRACE', 'LBRACKET', 'RBRACKET', 'LPAREN', 'RPAREN',
    'COMMA', 'SEMI', 'ASSIGN'
]


reserved = {
    'class': 'CLASS',
    'callout': 'CALLOUT',
    'break': 'BREAK',
    'else': 'ELSE',
    'continue': 'CONTINUE',
    'boolean': 'BOOLEAN',
    'if': 'IF',
    'int': 'INT',
    'return': 'RETURN',
    'true': 'TRUE',
    'false': 'FALSE',
    'while': 'WHILE',
    'void': 'VOID'
}

tokens = tokens + list(reserved.values())


t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_MOD = r'%'
t_EQ = r'=='
t_NEQ = r'!='
t_LE = r'<='
t_GE = r'>='
t_LT = r'<'
t_GT = r'>'
t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'!'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r','
t_SEMI = r';'
t_ASSIGN = r'='
t_ignore = ' \t\r\n'

def t_INT_LITERAL(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# parser part
def p_program(p):
    'program : CLASS ID LBRACE field_decl method_decl RBRACE'
    p[0] = ("program", p[2], p[4], p[5])

def p_field_decl(p):
    '''field_decl : type ID SEMI
                  | empty'''
    if len(p) == 4:
        p[0] = ("field", p[1], p[2])
    else:
        p[0] = None

def p_method_decl(p):
    '''method_decl : type ID LPAREN param_list RPAREN block
                   | VOID ID LPAREN param_list RPAREN block'''
    p[0] = ("method", p[1], p[2], p[4], p[6])

def p_type(p):
    '''type : INT
            | BOOLEAN'''
    p[0] = p[1]

def p_param_list(p):
    '''param_list : type ID
                  | empty'''
    if len(p) == 3:
        p[0] = [("param", p[1], p[2])]
    else:
        p[0] = []

def p_block(p):
    'block : LBRACE var_decl statement_list RBRACE'
    var_decl = p[2] if p[2] else []
    statement_list = p[3] if p[3] else []
    p[0] = ("block", var_decl, statement_list)

def p_var_decl(p):
    '''var_decl : type ID SEMI
                | empty'''
    if len(p) == 4:
        p[0] = [("var_decl", p[1], p[2])]
    else:
        p[0] = []

def p_statement_list(p):
    '''statement_list : statement statement_list
                      | empty'''
    if len(p) == 3:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = []

def p_statement(p):
    '''statement : location ASSIGN expr SEMI
                 | IF LPAREN expr RPAREN block ELSE block
                 | IF LPAREN expr RPAREN block
                 | RETURN expr SEMI
                 | empty'''
    if len(p) == 5 and p[2] == '=':
        p[0] = ("assign", p[1], p[3])
    elif len(p) == 8:
        p[0] = ("if_else", p[3], p[5], p[7])
    elif len(p) == 6 and p[1] == 'if':
        p[0] = ("if", p[3], p[5])
    elif len(p) == 3 and p[1] == 'return':
        p[0] = ("return", p[2])
    else:
        p[0] = None

def p_statement_error(p):
    '''statement : RETURN error SEMI'''
    print("Syntax error in return statement. Ensure the expression is valid.")
    p[0] = None

def p_location(p):
    '''location : ID
                | ID LBRACKET expr RBRACKET'''
    if len(p) == 2:
        p[0] = ("var", p[1])
    else:
        p[0] = ("array_var", p[1], p[3])

def p_expr(p):
    '''expr : location
            | INT_LITERAL
            | TRUE
            | FALSE
            | LPAREN expr RPAREN
            | expr PLUS expr
            | expr MINUS expr
            | expr TIMES expr
            | expr DIVIDE expr
            | expr AND expr
            | expr OR expr
            | NOT expr'''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        if p[1] == '(':
            p[0] = p[2]  # Parentheses
        else:
            p[0] = ("binop", p[2], p[1], p[3])
    elif len(p) == 3:
        p[0] = ("unop", p[1], p[2])

def p_empty(p):
    'empty :'
    p[0] = None

def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}'")
    else:
        print("Syntax error: bad end of input (EOF). check braces are closed.")

parser = yacc.yacc()

# === TESTING ===
test_cases = [
    '''
    class Class1 {
        in x;
        void main() {
            x = 10;
        }
    }
    ''',
    '''
    class Class2 {
        boolean flag;
        void toggle() {
            flag = true;
            if (flag) {
                flag = false;
            }
        }
    }
    ''',
    '''
    class Class3 {
        int x;
        void main() {
            x = 10;
    ''',
    '''
    class Class4 {
        int y;
        void func() {
            return 10 + ;
        }
    }
    ''',
    '''
    class Class5 {
        int z;
        void compute() {
            z = 5 * (2 + 3);
        }
    }
    '''
]

for i, data in enumerate(test_cases):
    print(f"\ntest: {i + 1}:")
    result = parser.parse(data, lexer=lexer)
    print(result)
