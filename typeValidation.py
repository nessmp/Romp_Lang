comparison = [">", "<", ">=", "<=", "==", "!="]
equal = "="
arithmetic = ["+", "-", "*", "/"]
logics = ["and", "or"]

comparisonReturn = {
    'integer': {
        'integer': 'bool',
        'real': 'bool',
    },
    'real': {
        'integer': 'bool',
        'real': 'bool'
    }
}

arithmeticReturn = {
    'integer': {
        'integer': 'integer',
        'real': 'real',
    },
    'real': {
        'integer': 'real',
        'real': 'real'
    }
}

equalReturn = {
    'integer': {
        'integer': 'integer',
    },
    'real': {
        'integer': 'real',
        'real': 'real',
    },
    'bool': {
        'bool': 'bool',
    }
}

logicReturn = {
    "bool" : {
        'bool': 'bool',
    }
}

def validType(operator, leftType, rightType):
    # print("operator leftType rightType", operator, leftType, rightType)
    if operator in comparison and leftType in comparisonReturn:
        try:
            return comparisonReturn[leftType][rightType]
        except:
            raise Exception(f'Bad Operation: {operator} with {leftType} - {rightType}')
    elif operator in arithmetic and leftType in arithmeticReturn:
        # print("arithmetic", arithmeticReturn[leftType][rightType])
        try:
            return arithmeticReturn[leftType][rightType]
        except:
            raise Exception(f'Bad Operation: {operator} with {leftType} - {rightType}')
    elif operator == "=" and leftType in equalReturn:
        try:
            return equalReturn[leftType][rightType]
        except:
            raise Exception(f'Bad Operation: {operator} with {leftType} - {rightType}')
    elif operator in logics and leftType in logicReturn:
        try:
            return logicReturn[leftType][rightType]
        except:
            raise Exception(f'Bad Operation: {operator} with {leftType} - {rightType}')
    raise Exception("Unknown operator")

def isBool(valueType):
    if valueType != "bool":
        raise Exception("Can't do that to non bool variable")