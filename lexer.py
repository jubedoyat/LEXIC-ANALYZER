import ply.lex as lex

# Palabras clave del lenguaje
keywords = {
    'stre', 'typ', 'procers', 'sikas', 'while_stre', 'again_stre',
    'por_stre_in', 'pasum_stre', 'jump_stre', 'in_stre', 'to_pasum_stre',
    'comeback', 'desdeU', 'sedef', 'comvar'
}

# Funciones reservadas específicas del dominio
functions = {
    'process_model_tways',
    'colectRpoint', 'colectadd', 'colectavg', 'colectaddB', 'colectaddT',
    'colectavgB', 'colectavgT', 'colectaddll', 'colectavgll',
    'order', 'compute', 'RetMtix'
}

# Tipos y literales
literals_reserved = {
    'siks': 'BOOLEAN', 'fals': 'BOOLEAN',
    'nimi': 'NULL', 'mtix': 'MATRIX', 'colect': 'SEQUENCE'
}

# Lista de tokens
tokens = [
    'IDENTIFIER', 'NUMBER_INT', 'NUMBER_FLOAT', 'STRING',
    'BOOLEAN', 'NULL', 'MATRIX', 'SEQUENCE',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MOD', 'POWER',
    'EQUALS', 'EQ', 'NE', 'LT', 'GT', 'LE', 'GE', 'IDENTITY',
    'AND', 'OR', 'NOT',
    'RANGE', 'SHIFT_LEFT', 'SHIFT_RIGHT', 'REPEAT', 'PERSIST', 'BREAK', 'LENRACHA', 'JOIN',
    'COMMA', 'SEMICOLON',
    'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'LANGLE', 'RANGLE', 'LDBRACE', 'RDBRACE',
    'LINE_COMMENT', 'BLOCK_COMMENT'
] + [f'KEYWORD_{kw.upper()}' for kw in keywords] + \
    [f'FUNC_{fn.upper()}' for fn in functions]

# Ignorar espacios y tabs
t_ignore = ' \t'

# Expresiones regulares para tokens simples
t_PLUS         = r'\+'
t_MINUS        = r'-'
t_TIMES        = r'\*'
t_DIVIDE       = r'/'
t_MOD          = r'%'
t_POWER        = r'\*\*|\^\^'
t_EQUALS       = r'='
t_EQ           = r'=='
t_NE           = r'!='
t_LT           = r'<'
t_GT           = r'>'
t_LE           = r'<='
t_GE           = r'>='
t_IDENTITY     = r'\?=\?'
t_AND          = r'yy'
t_OR           = r'oo'
t_NOT          = r'YO'
t_RANGE        = r':->|\.{3}'
t_SHIFT_LEFT   = r'<<<'
t_SHIFT_RIGHT  = r'>>>'
t_PERSIST      = r'~~>'
t_BREAK        = r'!!'
t_JOIN         = r'&join'
t_COMMA        = r','
t_SEMICOLON    = r';'
t_LPAREN       = r'\('
t_RPAREN       = r'\)'
t_LBRACKET     = r'\['
t_RBRACKET     = r'\]'
t_LANGLE       = r'<'
t_RANGLE       = r'>'
t_LDBRACE      = r'\{\{'
t_RDBRACE      = r'\}\}'

# Comentarios
def t_LINE_COMMENT(t):
    r'\-\/[^\n]*'
    pass

def t_REPEAT(t):
    r'\^\^\^'
    return t


def t_BLOCK_COMMENT(t):
    r'\-\/\/(.|\n)*?\/\-'
    pass

# Literales numéricos
def t_NUMBER_FLOAT(t):
    r'-?\d+\.\d+'
    t.value = float(t.value)
    return t

def t_NUMBER_INT(t):
    r'-?\d+'
    t.value = int(t.value)
    return t

# Cadenas de texto
def t_STRING(t):
    r'(\"([^\\\n]|(\\.))*?\")|(\'([^\\\n]|(\\.))*?\')'
    t.value = t.value[1:-1]  # Remueve comillas
    return t

# Identificadores, palabras clave y funciones especiales
def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'

    # Funciones reservadas
    if t.value in functions:
        t.type = 'FUNC_' + t.value.upper()
    # Literales reservados
    elif t.value in literals_reserved:
        t.type = literals_reserved[t.value]
        if t.type == 'BOOLEAN':
            t.value = True if t.value == 'siks' else False
        elif t.type == 'NULL':
            t.value = None
    # Palabras clave
    elif t.value in keywords:
        t.type = 'KEYWORD_' + t.value.upper()

    return t

def t_LENRACHA(t):
    r'\#stre\s*\(\s*[a-zA-Z_][a-zA-Z0-9_]*\s*\)'
    return t


# Conteo de líneas
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Cálculo de columna
def find_column(input, token):
    last_cr = input.rfind('\n', 0, token.lexpos)
    if last_cr < 0:
        last_cr = -1
    return token.lexpos - last_cr

# Manejo de errores con línea y columna
def t_error(t):
    col = find_column(t.lexer.lexdata, t)
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}, column {col}")
    t.lexer.skip(1)

# Construcción del lexer
lexer = lex.lex()
