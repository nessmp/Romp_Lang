import ply.lex as lex
import ply.yacc as yacc
import sys
from typeValidation import validType, isBool
from exec import execute

NUM_TEMP_VARIABLES = 50

quadruplets = []
quadrupletIndex = 1
operandsStack = []
operatorsStack = []
typesStack = []
jumpsStack = []
ifsStack = []
dosStack = []
exitsStack = []
readWriteVars = []
available = []

def peek(list):
    if len(list) == 0:
        return None
    return list[len(list) - 1]

for i in range(NUM_TEMP_VARIABLES):
    available.append('#' + str(i))

symbolsTableIndex = NUM_TEMP_VARIABLES

symbols = {}

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
    'comment',
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
t_comment = r'![a-zA-Z0-9_ ]*'
t_string = r'\'[a-zA-Z0-9 \.\?\:\t\r\n\f()\[\]\&\!\@\#\$\%\^\-\=\+\/\,]*\''
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
    P : program id VARIABLES ACTION_QUADRUPLE_GOTOMAIN SUBROUTINES ACTION_FILL_GOTO_MAIN STATEMENTS end program
    ''' 

def addSymbol(name, type, dimensions):
    global symbolsTableIndex
    if name in symbols:
        raise Exception(f'{name} already declared')
    if type == "subroutine":
        symbols[name] = {
            "startDirection" : quadrupletIndex
        }
    else:
        symbols[name] = {
            "type" : type,
            "value" : 0 if type == 'integer' else 0.0,
            "direction" : "#" + str(symbolsTableIndex)
            }
        if dimensions != None:
            if isinstance(dimensions, int):
                symbols[name]["rows"] = dimensions
                symbols[name]["reserved"] = "#" + str(int(symbols[name]["direction"][1:]) + dimensions)
                symbols[name]["reserved2"] = "#" + str(int(symbols[name]["direction"][1:]) + dimensions + 1)
                symbols[name]["usingReserved"] = False
                symbolsTableIndex += dimensions + 3
            elif len(dimensions) == 2:
                symbols[name]["rows"] = dimensions[0]
                symbols[name]["columns"] = dimensions[1]
                symbols[name]["reserved"] = "#" + str(int(symbols[name]["direction"][1:]) + dimensions[0] * dimensions[1])
                symbols[name]["reserved2"] = "#" + str(int(symbols[name]["direction"][1:]) + dimensions[0] * dimensions[1] + 1)
                symbols[name]["usingReserved"] = False
                symbolsTableIndex += dimensions[0] * dimensions[1] + 3
        else:
            symbolsTableIndex += 1

def p_variables(p):
    '''
    VARIABLES : TYPE id ARRAY semicolon VARIABLES
              | 
    '''
    if len(p) == 6:
        addSymbol(p[2], p[1], p[3])

def p_type(p):
    '''
    TYPE : integer
         | real
    '''
    p[0] = p[1]

def p_array(p):
    '''
    ARRAY : openBracket ARITEXP closeBracket openBracket ARITEXP closeBracket
          | openBracket ARITEXP closeBracket
          |
    '''
    if len(p) == 7:
        p[0] = [operandsStack.pop(), operandsStack.pop()]
        typesStack.pop()
        typesStack.pop()
    elif len(p) == 4:
        p[0] = operandsStack.pop()
        typesStack.pop()

def p_subroutines(p):
    '''
    SUBROUTINES : subroutine id ACTION_ADD_TO_TABLE STATEMENTS ACTION_QUADRUPLE_GOBACK end subroutine SUBROUTINES
                |
    '''

def p_statements(p):
    '''
    STATEMENTS : if LOGEXP ACTION_QUADRUPLE_EMPTY_JUMP then STATEMENTS ACTION_NEW_IF ACTION_QUADRUPLE_GOTO_ENDIF ELIF ELSE end if ACTION_FILL_GOTO_ENDIF STATEMENTS
               | DO
               | VAR equal ARITEXP ACTION_QUADRUPLET_SET STATEMENTS
               | call id ACTION_QUADRUPLE_CALL STATEMENTS
               | read READVAR ACTION_QUADRUPLE_READ STATEMENTS
               | write WRITEVAR ACTION_QUADRUPLE_WRITE STATEMENTS
               | exit ACTION_QUADRUPLE_EXITSSTACK STATEMENTS
               | comment STATEMENTS
               |
    '''

def p_(p):
    '''
    DO : do ACTION_PUSH_FLAG_EXITSSTACK then ACTION_PUSH_DOSSTACK STATEMENTS ACTION_GOTO_DO end do ACTION_FILL_EXITS_JUMPS STATEMENTS
       | do id equal ARITEXP ACTION_QUADRUPLET_SET coma ACTION_PUSH_DOSSTACK LOGEXP ACTION_QUADRUPLE_EMPTY_JUMP then STATEMENTS ACTION_QUADRUPLE_ADD_TO_COUNTER ACTION_GOTO_DO end do ACTION_FILL_JUMP STATEMENTS
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
               | ARITEXP COMP ARITEXP ACTION_QUADRUPLE_COMP_COMPARISON
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
    readWriteVars.append(p[1])

def p_readv(p):
    '''
    READV : coma VAR READV
          |
    '''
    if len(p) == 4:
        readWriteVars.append(p[2])

def p_writevar(p):
    '''
    WRITEVAR : VAR WRITEV
             | string WRITEV
    '''
    readWriteVars.append(p[1])

def p_writev(p):
    '''
    WRITEV : coma VAR WRITEV
           | coma string WRITEV
           |
    '''
    if len(p) == 4:
        readWriteVars.append(p[2])

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
    VAR : id ARRAY ACTION_QUADRUPLE_ARRAY
    '''
    if p[2] == None:
        p[0] = p[1]
    else:
        p[0] = [p[1], p[3]]

def p_action_var_val(p):
    "ACTION_VAR_VAL :"
    if isinstance(p[-1], list):
        # print(p[-1])
        if symbols[p[-1][0]]["reserved"] == p[-1][1] or symbols[p[-1][0]]["reserved2"] == p[-1][1]:
            operandsStack.append("*" + p[-1][1][1:])
        else:
            operandsStack.append(p[-1][1])
        typesStack.append(symbols[p[-1][0]]["type"])
    else:
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
    variableType = ""
    variableDirection = ""
    if isinstance(variable, list):
        variableType = symbols[variable[0]]["type"]
        if symbols[variable[0]]["reserved"] == variable[1] or symbols[variable[0]]["reserved2"] == variable[1]:
            variableDirection = "*" + variable[1][1:]
        else:
            variableDirection = variable[1]
    else:
        variableType = symbols[variable]["type"]
        variableDirection = symbols[variable]["direction"]
    value = operandsStack.pop()
    valueType = typesStack.pop()
    # print(p[-1])
    validType(operator, variableType, valueType)
    quadruplets.append(str(operator) + ' ' + str(value) + ' ' + str(variableDirection))
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
    quadruplets.append(str(operator) + ' ' + str(leftOperand) + ' ' + str(rightOperand) + ' ' + str(temp))
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
    quadruplets.append("not " + str(value) + ' ' + str(temp))
    global quadrupletIndex
    quadrupletIndex += 1

def p_action_quadruple_empty_jump(p):
    "ACTION_QUADRUPLE_EMPTY_JUMP :"
    global quadrupletIndex
    value = quadruplets[quadrupletIndex - 2].split()
    # print("ACTION_QUADRUPLE_EMPTY_JUMP", value[len(value) - 1])
    quadruplets.append("gotoF " + str(value[len(value) - 1]) + ' ')
    jumpsStack.append(quadrupletIndex)
    quadrupletIndex += 1

def fillJump(quadrupletsIndex, goto):
    # print("fillJump", quadrupletsIndex, goto)
    quadruplets[quadrupletsIndex] = quadruplets[quadrupletsIndex] + str(goto)

def p_action_fill_jump(p):
    "ACTION_FILL_JUMP :"
    # print("jumpsStack", jumpsStack)
    fillJump(jumpsStack.pop()- 1, quadrupletIndex)

def p_action_quadruple_goto_endif(p):
    "ACTION_QUADRUPLE_GOTO_ENDIF :"
    global quadrupletIndex
    ifsStack[len(ifsStack) - 1].append(quadrupletIndex)
    # print(ifsStack)
    quadruplets.append("goto ")
    quadrupletIndex += 1

def p_new_if(p):
    "ACTION_NEW_IF :"
    ifsStack.append([])

def p_action_fill_goto_endif(p):
    "ACTION_FILL_GOTO_ENDIF :"
    for goto in ifsStack[len(ifsStack) - 1]:
        fillJump(goto - 1, quadrupletIndex)
    ifsStack.pop()

def p_action_push_dosstack(p):
    "ACTION_PUSH_DOSSTACK :"
    dosStack.append(quadrupletIndex)

def p_action_goto_do(p):
    "ACTION_GOTO_DO :"
    quadruplets.append("goto" + ' ' + str(dosStack.pop()))
    global quadrupletIndex
    quadrupletIndex += 1

def p_action_quadruple_add_to_counter(p):
    "ACTION_QUADRUPLE_ADD_TO_COUNTER :"
    quadruplets.append("+ 1 " + str(symbols[p[-10]]["direction"]) + ' ' + str(symbols[p[-10]]["direction"]))
    global quadrupletIndex
    quadrupletIndex += 1

def p_action_push_flag_exitsstack(p):
    "ACTION_PUSH_FLAG_EXITSSTACK :"
    exitsStack.append('-')

def p_action_quadruple_exitsstack(p):
    "ACTION_QUADRUPLE_EXITSSTACK :"
    quadruplets.append("goto ")
    global quadrupletIndex
    exitsStack.append(quadrupletIndex)
    quadrupletIndex += 1

def p_action_fill_exits_jumps(p):
    "ACTION_FILL_EXITS_JUMPS :"
    index = exitsStack.pop()
    while index != '-':
        fillJump(index - 1, quadrupletIndex)
        index = exitsStack.pop()

def p_action_quadruple_array(p):
    "ACTION_QUADRUPLE_ARRAY :"
    global quadrupletIndex
    if p[-1] != None:
        if "reserved" not in symbols[p[-2]] or (isinstance(p[-1], int) and "columns" in symbols[p[-2]]):
            raise Exception(f"{p[-2]} is not an array or matrix")
        if isinstance(p[-1], int):
            p[0] = "#" + str(p[-1] + int(symbols[p[-2]]["direction"][1:]))
        elif isinstance(p[-1], list):
            quadruplets.append("* " + str(p[-1][0]) + " " + str(symbols[p[-2]]["columns"]) + " " + str(symbols[p[-2]]["reserved"]))
            quadruplets.append("+ " + str(p[-1][1]) + " " + str(symbols[p[-2]]["reserved"]) + " " + str(symbols[p[-2]]["reserved"]))
            quadruplets.append("+ " + str(symbols[p[-2]]["direction"][1:]) + " " + str(symbols[p[-2]]["reserved"]) + " " + str(symbols[p[-2]]["reserved"]))
            quadrupletIndex += 3
            p[0] = str(symbols[p[-2]]["reserved"])
        else:
            if symbols[p[-2]]["usingReserved"]:
                quadruplets.append("+ " + str(symbols[p[-2]]["direction"][1:]) + " " + str(p[-1]) + " " + str(symbols[p[-2]]["reserved2"]))
                quadrupletIndex += 1
                p[0] = str(symbols[p[-2]]["reserved2"])
                symbols[p[-2]]["usingReserved"] = False
            else:
                quadruplets.append("+ " + str(symbols[p[-2]]["direction"][1:]) + " " + str(p[-1]) + " " + str(symbols[p[-2]]["reserved"]))
                quadrupletIndex += 1
                p[0] = str(symbols[p[-2]]["reserved"])
                symbols[p[-2]]["usingReserved"] = True

def p_action_quadruple_gotomain(p):
    "ACTION_QUADRUPLE_GOTOMAIN :"
    quadruplets.append("goto " )
    global quadrupletIndex
    p[0] = quadrupletIndex
    quadrupletIndex += 1

def p_action_fill_gotomain(p):
    "ACTION_FILL_GOTO_MAIN :"
    quadruplets[p[-2] - 1] += str(quadrupletIndex)

def p_action_add_to_table(p):
    "ACTION_ADD_TO_TABLE :"
    addSymbol(p[-1], "subroutine", None)

def p_action_quadruple_call(p):
    "ACTION_QUADRUPLE_CALL :"
    if p[-1] not in symbols:
        raise Exception(f"subroutine {p[-1]} not declared")
    quadruplets.append("call " + str(symbols[p[-1]]["startDirection"]))
    global quadrupletIndex
    quadrupletIndex += 1

def p_action_quadruple_goback(p):
    "ACTION_QUADRUPLE_GOBACK :"
    quadruplets.append("goback ")
    global quadrupletIndex
    quadrupletIndex += 1

def p_action_quadruple_read(p):
    "ACTION_QUADRUPLE_READ :"
    vars = ""
    global readWriteVars
    for var in readWriteVars:
        if isinstance(var, list):
            if symbols[var[0]]["reserved"] == var[1] or symbols[var[0]]["reserved2"] == var[1]:
                vars = "*"  + var[1][1:] + " " + vars
            else:
                vars = var[1] + vars
        else:
            vars = symbols[var]["direction"] + " " + vars

    quadruplets.append("read " + vars)
    global quadrupletIndex
    quadrupletIndex += 1
    readWriteVars = []

def p_action_quadruple_write(p):
    "ACTION_QUADRUPLE_WRITE :"
    vars = ""
    global readWriteVars
    for var in readWriteVars:
        if isinstance(var, list):
            if symbols[var[0]]["reserved"] == var[1] or symbols[var[0]]["reserved2"] == var[1]:
                vars = "*"  + var[1][1:] + " " + vars
            else:
                vars = var[1] + vars
        else:
            if var in symbols:
                vars = symbols[var]["direction"] + " " + vars
            else:
                vars = var + " " + vars

    quadruplets.append("write " + vars)
    global quadrupletIndex
    quadrupletIndex += 1
    readWriteVars = []

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
        print(i, "\t", quadruplet)
        i += 1
    print()
    execute(quadruplets, symbols, NUM_TEMP_VARIABLES)
else:
    raise Exception('''
    No file name was provided.]
    Example: romp.py test.rmop
    ''')