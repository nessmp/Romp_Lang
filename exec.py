import ast

callsStack = []

class Variables:
    def __init__(self):
        self.variables = {}
    
    def set(self, direction, value, type = None):
        if direction[0] == "#":
            direction = direction[1:]
        elif direction[0] == "*":
            direction = int(self.get("#" + direction[1:]))

        if type == None:
            if isinstance(value, str):
                    value = ast.literal_eval(value)
            if self.variables[direction]["type"] == "integer":
                value = int(value)
            self.variables[direction]["value"] = value
        else:
            self.variables[direction] = {"value" : value, "type" : type}

    def get(self, direction):
        if direction[0] == "#":
            direction = direction[1:]
        elif direction[0] == "*":
            if direction[1:] in self.variables:
                direction = self.variables[direction[1:]]
            else:
                raise Exception("Error: Access before declaratios")

        if direction in self.variables:
            return self.variables[direction]
        else:
            raise Exception("Error: Access before declaratios")

variables = Variables()

def getQuadruple(quadruple):
    splitQuadruple = []
    i = 0
    while i < len(quadruple):
        quadruplePart = ""
        if quadruple[i] == "\'":
            quadruplePart += quadruple[i]
            i+= 1
            while quadruple[i] != "\'" and quadruple[i - 1] != "\\":
                quadruplePart += quadruple[i]
                i+= 1
            quadruplePart += quadruple[i]
            i+= 1
            splitQuadruple.append(quadruplePart)
        else:
            while i < len(quadruple) and quadruple[i] != " ":
                quadruplePart += quadruple[i]
                i += 1
            i += 1
            if quadruplePart != "":
                splitQuadruple.append(quadruplePart)            
    
    return splitQuadruple

def isDirection(data):
    try:
        if data[0] == "#" or data[0] == '*':
            return True
        return False
    except:
        return False

def doOperation(operator, operand1, operand2):
    if isDirection(operand1):
        operand1 = variables.get(operand1)["value"]
    else:
        operand1 = ast.literal_eval(operand1)
    if isDirection(operand2):
        operand2 = variables.get(operand2)["value"]
    else:
        operand2 = ast.literal_eval(operand2)
    
    if operator == '+':
        return operand1 + operand2
    elif operator == '-':
        return operand1 - operand2
    elif operator == '*':
        return operand1 * operand2
    elif operator == '/':
        return operand1 / operand2
    elif operator == '>':
        return operand1 > operand2
    elif operator == '<':
        return operand1 < operand2
    elif operator == '>=':
        return operand1 >= operand2
    elif operator == '<=':
        return operand1 <= operand2
    elif operator == '==':
        return operand1 == operand2
    elif operator == '!=':
        return operand1 != operand2
    elif operator == 'and':
        return operand1 and operand2
    elif operator == 'or':
        return operand1 or operand2

def executeQuadruple(quadruple, currentQuadruple):
    if quadruple[0] == "write":
        for data in quadruple[1:]:
            if data[0] == "\'":
                print(data.replace('\'', ''), end="")
            else:
                print(variables.get(data)["value"], end="")
        print()
    elif quadruple[0] == "read":
        for data in quadruple[1:]:
            variables.set(data, input())
    elif quadruple[0] in ['+', '-', '*', '/', '==', '!=', '<=', '>=', '<', '>', 'and', 'or']:
        variables.set(quadruple[3], doOperation(quadruple[0], quadruple[1], quadruple[2]))
    elif quadruple[0] == "not":
        variables.set(quadruple[1], not variables.get(quadruple[1]))
    elif quadruple[0] == "=":
        if isDirection(quadruple[1]):
            variables.set(quadruple[2], variables.get(quadruple[1])["value"])
        else:
            variables.set(quadruple[2], ast.literal_eval(quadruple[1]))
    elif quadruple[0] == "gotoF":
        if not variables.get(quadruple[1])["value"]:
            return ast.literal_eval(quadruple[2])
    elif quadruple[0] == "goto":
        return ast.literal_eval(quadruple[1])
    elif quadruple[0] == "call":
        callsStack.append(currentQuadruple + 1)
        return ast.literal_eval(quadruple[1])
    elif quadruple[0] == "goback":
        return callsStack.pop()

    return currentQuadruple + 1

def execute(quadruplets, symbols, numTempVariables):
    for i in range(numTempVariables):
        variables.set("#" + str(i), 0, "temp")
    for _, value in symbols.items():
        if "direction" in value:
            if "reserved" in value:
                for i in range(int(value["direction"][1:]), int(value["reserved"][1:]) + 1):
                    variables.set("#" + str(i), value["value"], value["type"])
            else:
                variables.set(value["direction"], value["value"], value["type"])
    currentQuadruple = 1
    while currentQuadruple <= len(quadruplets):
        currentQuadruple = executeQuadruple(getQuadruple(quadruplets[currentQuadruple - 1]), currentQuadruple)