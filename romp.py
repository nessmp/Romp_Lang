import ply.lex as lex
import ply.yacc as yacc
import sys
from typeValidation import validType, isBool

NUM_TEMP_VARIABLES = 50

quadruplets = []
quadrupletIndex = 1
operandsStack = []
operatorsStack = []
typesStack = []
jumpsStack = []
ifsStack = []
exitStack = []
available = []

def peek(list):
    if len(list) == 0:
        return None
    return list[len(list) - 1]

for i in range(NUM_TEMP_VARIABLES):
    available.append('#' + str(i))

currentIndex = NUM_TEMP_VARIABLES

symbols = {}
def addSymbol(name, type):
    global currentIndex
    symbols[name] = {
        "type" : type,
        "value" : 0 if type == 'integer' else 0.0,
        "direction" : "#" + str(currentIndex)
        }
    currentIndex += 1

tokens = [
    'id',
    'semicolon',
    'openBracket',
    'closeBracket',
    'openParentheses',
    'closeParentheses',
    'doubleEqual',
    'notEqual',
    'biggerOrEqualThan',
    'smallerOrEqualThan',
    'biggerThan',
    'smallerThan',
    'equal',
    'coma',
    'string',
    'plusSign',
    'minusSign',
    'multSign',
    'divSign',
    #Reserved Tokens
    'program',
    'end',
    'read',
    'write',
    'if',
    'then',
    'else',
    'elif',
    'do',
    'exit',
    'integer',
    'int',
    'real',
    'subroutine',
    'call',
    'or',
    'and',
    'not',

]

reserved = {
    'program' : 'program',
    'end' : 'end',
    'read' : 'read',
    'write' : 'write',
    'if' : 'if',
    'then' : 'then',
    'else' : 'else',
    'elif' : 'elif',
    'do' : 'do',
    'exit' : 'exit',
    'integer' : 'integer',
    'real' : 'real',
    'subroutine' : 'subroutine',
    'call' : 'call',
    'or' : 'or',
    'and' : 'and',
    'not' : 'not',
}

t_semicolon = r';' 
t_openBracket = r'\['
t_closeBracket = r'\]'
t_or = r'or'
t_and = r'and'
t_not = r'not'
t_openParentheses = r'\('
t_closeParentheses = r'\)'
t_doubleEqual = r'\=\='
t_notEqual = r'\!\='
t_biggerOrEqualThan = r'\>\='
t_smallerOrEqualThan = r'\<\='
t_biggerThan = r'\>'
t_smallerThan = r'\<'
t_equal = r'\='
t_coma = r','
t_string = r'\'[a-zA-Z0-9 \t\r\n\f()\[\]\&\!\@\#\$\%\^\-\=\+\/\,]*\''
t_plusSign = r'\+'
t_minusSign = r'-'
t_multSign = r'\*'
t_divSign = r'\/'
t_ignore = ' \t\r\n\f\v'

def t_real(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_int(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_id(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if t.value in reserved:
        t.type = reserved[ t.value ]
    else:  
        t.type = 'id'
    return t

def t_error(t):
    print("Illegal character!", t)
    t.lexer.skip(1)

lexer = lex.lex()

def p_P(p):
    '''
    P : program id VARIABLES SUBROUTINES STATEMENTS end program
    ''' 

def p_variables(p):
    '''
    VARIABLES : TYPE id ARRAY semicolon VARIABLES
              | 
    '''
    if len(p) == 6:
        addSymbol(p[2], p[1])

def p_type(p):
    '''
    TYPE : integer
         | real
    '''
    p[0] = p[1]

def p_array(p):
    '''
    ARRAY : openBracket int closeBracket openBracket int closeBracket
          | openBracket int closeBracket
          | openBracket id closeBracket openBracket id closeBracket
          | openBracket id closeBracket
          |
    '''

def p_subroutines(p):
    '''
    SUBROUTINES : subroutine id STATEMENTS end subroutine SUBROUTINES
                |
    '''

def p_statements(p):
    '''
    STATEMENTS : if LOGEXP ACTION_QUADRUPLE_EMPTY_JUMP then STATEMENTS ACTION_NEW_IF ACTION_QUADRUPLE_GOTO_ENDIF ELIF ELSE end if ACTION_FILL_GOTO_ENDIF STATEMENTS
               | do STATEMENTS end do STATEMENTS
               | do id equal ARITEXP ACTION_QUADRUPLET_SET coma ARITEXP ACTION_QUADRUPLE_DO_LIMIT STATEMENTS end do STATEMENTS
               | VAR equal ARITEXP ACTION_QUADRUPLET_SET STATEMENTS
               | call id STATEMENTS
               | read READVAR STATEMENTS
               | write WRITEVAR STATEMENTS
               | exit STATEMENTS
               |
    '''
def p_elif(p):
    '''
    ELIF : elif ACTION_FILL_JUMP LOGEXP ACTION_QUADRUPLE_EMPTY_JUMP then STATEMENTS ACTION_QUADRUPLE_GOTO_ENDIF ELIF
         |
    '''

def p_else(p):
    '''
    ELSE : else ACTION_FILL_JUMP STATEMENTS ACTION_QUADRUPLE_GOTO_ENDIF
         | ACTION_FILL_JUMP
    '''

def p_logexp(p):
    '''
    LOGEXP : LOGEXP or ACTION_OR_LOGEXP ANDEXP ACTION_CREATE_QUADRUPLE_LOGEXP
           | ANDEXP
    '''

def p_andexp(p):
    '''
    ANDEXP : ANDEXP and ACTION_AND_ANDEXP COMPARISON ACTION_QUADRUPLE_ANDEXP 
           | COMPARISON
    '''

def p_comparison(p):
    '''
    COMPARISON : openParentheses LOGEXP closeParentheses
               | VALUE COMP VALUE ACTION_QUADRUPLE_COMP_COMPARISON
               | not LOGEXP ACTION_QUADRUPLE_NOT_COMPARISON
    '''

def p_comp(p):
    '''
    COMP : doubleEqual
         | notEqual
         | biggerOrEqualThan
         | smallerOrEqualThan
         | biggerThan
         | smallerThan
    '''
    operatorsStack.append(p[1])
    p[0] = p[1]

def p_readvar(p):
    '''
    READVAR : VAR READV
    '''

def p_readv(p):
    '''
    READV : coma VAR READV
          |
    '''

def p_writevar(p):
    '''
    WRITEVAR : VAR WRITEV
             | string WRITEV
    '''

def p_writev(p):
    '''
    WRITEV : coma VAR WRITEV
           | coma string WRITEV
           |
    '''


def p_aritexp(p):
    '''
    ARITEXP : MULDIV
            | ARITEXP plusSign ACTION_PLUSSIGN_ARITEXP MULDIV ACTION_QUADRUPLET_ARITEXP
            | ARITEXP minusSign ACTION_MINUSSIGN_ARITEXP MULDIV ACTION_QUADRUPLET_ARITEXP
    '''

def p_muldiv(p):
    '''
    MULDIV : VALUE
           | MULDIV multSign ACTION_MULTSIGN_MULDIV VALUE ACTION_QUADRUPLET_MULDIV
           | MULDIV divSign ACTION_DIVSIGN_MULDIV VALUE ACTION_QUADRUPLET_MULDIV
    '''
    p[0] = p[1]

def p_value(p):
    '''
    VALUE : VAL
          | openParentheses ARITEXP closeParentheses
    '''

def p_val(p):
    '''
    VAL : VAR ACTION_VAR_VAL
        | int ACTION_INT_VAL
        | real ACTION_REAL_VAL
    '''

def p_var(p):
    '''
    VAR : id ARRAY
    '''
    p[0] = p[1]

def p_action_var_val(p):
    "ACTION_VAR_VAL :"
    operandsStack.append(symbols[p[-1]]["direction"])
    typesStack.append(symbols[p[-1]]["type"])

def p_action_int_val(p):
    "ACTION_INT_VAL :"
    # print("int_val", p[-1])
    operandsStack.append(p[-1])
    typesStack.append("integer")

def p_action_real_val(p):
    "ACTION_REAL_VAL :"
    # print("real_val", p[-1])
    operandsStack.append(p[-1])
    typesStack.append("real")

def p_action_plussign_aritexp(p):
    "ACTION_PLUSSIGN_ARITEXP :"
    # print("plusSign", p[-1])
    operatorsStack.append(p[-1])

def p_action_minussign_aritexp(p):
    "ACTION_MINUSSIGN_ARITEXP :"
    # print("minusSign", p[-1])
    operatorsStack.append(p[-1])


def p_action_quadruplet_set(p):
    "ACTION_QUADRUPLET_SET :"
    operator = p[-2]
    variable = p[-3]
    variableType = symbols[variable]["type"]
    variableDirection = symbols[variable]["direction"]
    value = operandsStack.pop()
    valueType = typesStack.pop()
    validType(operator, variableType, valueType)
    quadruplets.append(str(operator) + ' ' + str(value) + ' ' + str(variableDirection) + '\n')
    global quadrupletIndex
    quadrupletIndex += 1

def p_action_multsign_muldiv(p):
    "ACTION_MULTSIGN_MULDIV :"
    operatorsStack.append(p[-1])

def p_action_divsign_muldiv(p):
    "ACTION_DIVSIGN_MULDIV :"
    operatorsStack.append(p[-1])

def addQuadruplet():
    operator = operatorsStack.pop()
    rightOperand = operandsStack.pop()
    rightOperandType = typesStack.pop()
    leftOperand = operandsStack.pop()
    leftOperandType = typesStack.pop()
    typesStack.append(validType(operator, leftOperandType, rightOperandType))
    temp = available.pop(0)
    quadruplets.append(str(operator) + ' ' + str(leftOperand) + ' ' + str(rightOperand) + ' ' + str(temp) + '\n')
    global quadrupletIndex
    quadrupletIndex += 1
    operandsStack.append(temp)

def p_action_quadruplet_aritexp(p):
    "ACTION_QUADRUPLET_ARITEXP :"
    operator = peek(operatorsStack) 
    # print("quadruplet aritexpt operator list", operatorsStack)
    if operator == "+" or operator == "-":
        addQuadruplet()

def p_action_quadruplet_muldiv(p):
    "ACTION_QUADRUPLET_MULDIV :"
    operator = peek(operatorsStack) 
    if operator == "*" or operator == "/":
        addQuadruplet()     

def p_action_or_logexp(p):
    "ACTION_OR_LOGEXP :"
    operatorsStack.append(p[-1])

def p_action_and_andexp(p):
    "ACTION_AND_ANDEXP :"
    operatorsStack.append(p[-1])

def p_action_create_quadruple_logexp(p):
    "ACTION_CREATE_QUADRUPLE_LOGEXP :"
    operator = peek(operatorsStack)
    if operator == "or":
        addQuadruplet()

def p_action_quadruple_andexp(p):
    "ACTION_QUADRUPLE_ANDEXP :"
    operator = peek(operatorsStack)
    if operator == "and":
        addQuadruplet()

def p_action_quadruple_comp_comparison(p):
    "ACTION_QUADRUPLE_COMP_COMPARISON :"
    addQuadruplet()

def p_action_quadruple_not_comparison(p):
    "ACTION_QUADRUPLE_NOT_COMPARISON :"
    value = operandsStack.pop()
    valueType = typesStack.pop()
    isBool(valueType)
    temp = available.pop(0)
    quadruplets.append(str("not") + ' ' + str(value) + ' ' + str(temp) +'\n')
    global quadrupletIndex
    quadrupletIndex += 1

def p_action_quadruple_empty_jump(p):
    "ACTION_QUADRUPLE_EMPTY_JUMP :"
    global quadrupletIndex
    value = quadruplets[quadrupletIndex - 2].split()
    # print("ACTION_QUADRUPLE_EMPTY_JUMP", value[len(value) - 1])
    quadruplets.append(str("gotoF") + ' ' + str(value[len(value) - 1]) + ' ')
    jumpsStack.append(quadrupletIndex)
    quadrupletIndex += 1

def fillJump(quadrupletsIndex, goto):
    # print("fillJump", quadrupletsIndex, goto)
    quadruplets[quadrupletsIndex] = quadruplets[quadrupletsIndex] + str(goto) + '\n'

def p_action_fill_jump(p):
    "ACTION_FILL_JUMP :"
    # print("jumpsStack", jumpsStack)
    fillJump(jumpsStack.pop()- 1, quadrupletIndex)

def p_action_quadruple_goto_endif(p):
    "ACTION_QUADRUPLE_GOTO_ENDIF :"
    global quadrupletIndex
    ifsStack[len(ifsStack) - 1].append(quadrupletIndex)
    # print(ifsStack)
    quadruplets.append(str("goto") + ' ')
    quadrupletIndex += 1

def p_new_if(p):
    "ACTION_NEW_IF :"
    ifsStack.append([])

def p_action_fill_goto_endif(p):
    "ACTION_FILL_GOTO_ENDIF :"
    for goto in ifsStack[len(ifsStack) - 1]:
        fillJump(goto - 1, quadrupletIndex)
    ifsStack.pop()

def p_action_quadruple_do_limit(p):
    "ACTION_QUADRUPLE_DO_LIMIT :"
    

def p_error(p):
    raise Exception(f'Wrong Syntax {p}')

parser = yacc.yacc()

if (len(sys.argv) > 1):
    programName = sys.argv[1]
    programFile = open(programName, "r")
    # This is neccessary because the read method parses literal ends
    #  of lines as \\n instead of \n.
    program = programFile.read().replace('\\n', '\n')
    parser.parse(program)
    programFile.close()
    i = 1
    for quadruplet in quadruplets:
        print(i, "\t", quadruplet.replace("\n", ""))
        i += 1
else:
    raise Exception('''
    No file name was provided.
    Please add the file name as a command line argument
    Example: romp.py test.rmop
    ''')