import ply.lex as lex

# list tokens
tokens = (
    'CLASS',
    'ID', 'INT_LITERAL', 'CHAR_LITERAL', 'BOOL_LITERAL', 'STRING_LITERAL',
    'HEX_LITERAL', 'ALPHA_NUM',
    'ADD_OP', 'SUB_OP', 'MUL_OP', 'DIV_OP', 'MOD_OP',
    'EQ_OP',
    'NEQ_OP', 'LESS_OP', 'GREATER_OP', 'LESS_EQ_OP', 'GREATER_EQ_OP',
    'AND_OP', 'OR_OP',
    'LEFT_CURLY', 'RIGHT_CURLY', 'LEFT_PAREN', 'RIGHT_PAREN',
    'SEMICOLON', 'COMMA',
    'IF', 'ELSE', 'WHILE', 'RETURN', 'BREAK', 'CONTINUE', 'CALLOUT',
    'ASSIGN_OP', 'NEWLINE', 'LEFT_SHIFT',
    'RIGHT_SHIFT',
    'UNSIGNED_RIGHT_SHIFT',
)

# re
t_CLASS = r'class'
t_IF = r'if'
t_ELSE = r'else'
t_WHILE = r'while'
t_RETURN = r'return'
t_BREAK = r'break'
t_CONTINUE = r'continue'
t_CALLOUT = r'callout'

t_ASSIGN_OP = r'='

t_ADD_OP = r'\+'
t_SUB_OP = r'-'
t_MUL_OP = r'\*'
t_DIV_OP = r'/'
t_MOD_OP = r'%'
t_AND_OP = r'&&'
t_OR_OP = r'\|\|'

t_EQ_OP = r'=='
t_NEQ_OP = r'!='
t_LESS_OP = r'<'
t_GREATER_OP = r'>'
t_LESS_EQ_OP = r'<='
t_GREATER_EQ_OP = r'>='

t_LEFT_SHIFT = r'<<'
t_RIGHT_SHIFT = r'>>'
t_UNSIGNED_RIGHT_SHIFT = r'>>>'

t_LEFT_CURLY = r'\{'
t_RIGHT_CURLY = r'\}'
t_LEFT_PAREN = r'\('
t_RIGHT_PAREN = r'\)'
t_SEMICOLON = r';'
t_COMMA = r','

t_ID = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_INT_LITERAL = r'\d+'
t_HEX_LITERAL = r'0x[0-9A-Fa-f]+'
t_CHAR_LITERAL = r'\'[^\']\''
t_BOOL_LITERAL = r'true|false'
t_STRING_LITERAL = r'\".*?\"'
t_ALPHA_NUM = r'[a-zA-Z0-9_]+'


# new line
def t_NEWLINE(t):
    r'\n+'
    #add to line number
    t.lexer.lineno += len(t.value)
    #ignore token
    pass

# ignore space
t_ignore = ' \t'

# error handling
def t_error(t):
    print(f"Unexpected character: {t.value[0]}")
    t.lexer.skip(1)

# initialize lexer
lexer = lex.lex()

# test lexer
if __name__ == '__main__':
    test_data = [
        '''class Test1 {  
            int value = 10;  
            if (value < 20) {  
                return value + 1;  
            } else {  
                return value - 1;  
            }  
        }''',

        '''class Test2 {  
            void main() {  
                int hexValue = 0x1F;  
                return hexValue;  
            }  
        }''',

        '''class Test3 {  
            void loop() {  
                for (int i = 0; i < 10; i++) {  
                    if (i % 2 == 0) {  
                        continue;  
                    } else {  
                        return i;  
                    }  
                }  
            }  
        }''',

        '''class Test4 {  
            bool test(int num) {  
                return (num > 0) && (num < 100);  
            }  
        }''',

        '''class Test5 {  
            int example(int value) {  
                return value << 1;  
                return value >> 1;  
                return value >>> 1;  
            }  
        }'''
    ]
    #print class
    for data in test_data:
        print("Testing Input:\n", data)
        lexer.input(data)

        #print token in lexer
        for tok in lexer:
            print(tok)
            #print \n , 50 times __, and \n
        print("\n" + "__"*50 + "\n")
