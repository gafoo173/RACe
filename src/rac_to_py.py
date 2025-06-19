import sys
import re

# 1. Lexical Analysis (Tokenization)
#    This phase reads the code as a stream of characters and groups them into "tokens".
#    Tokens are the smallest meaningful units in the language.

# Types of tokens that the lexer can recognize
class TokenType:
    # Keywords
    INT = 'INT'
    PRINT = 'PRINT'
    IF = 'IF'
    ELSE = 'ELSE'
    WHILE = 'WHILE'
    LOGIC = 'LOGIC'
    FLOAT = 'FLOAT'
    FUNC = 'FUNC'
    RETURN = 'RETURN'
    TRUE = 'TRUE'
    FALSE = 'FALSE'

    # Identifiers (variable names, function names, class names)
    IDENTIFIER = 'IDENTIFIER'

    # Numbers
    NUMBER = 'NUMBER'
    NUMBER_FLOAT = 'NUMBER_FLOAT'

    # String literals
    STRING = 'STRING'

    # Operators
    ASSIGN = 'ASSIGN'             # =
    PLUS = 'PLUS'                 # +
    MINUS = 'MINUS'               # -
    MULTIPLY = 'MULTIPLY'         # *
    DIVIDE = 'DIVIDE'             # /

    # Comparison Operators
    EQUALEQUAL = 'EQUALEQUAL'     # ==
    NOT_EQUAL = 'NOT_EQUAL'       # !=
    LESS = 'LESS'                 # <
    GREATER = 'GREATER'           # >
    LESS_EQUAL = 'LESS_EQUAL'     # <=
    GREATER_EQUAL = 'GREATER_EQUAL' # >=

    # Delimiters and end-of-file
    PAREN_OPEN = 'PAREN_OPEN'     # (
    PAREN_CLOSE = 'PAREN_CLOSE'   # )
    BRACE_OPEN = 'BRACE_OPEN'     # {
    BRACE_CLOSE = 'BRACE_CLOSE'   # }
    SEMICOLON = 'SEMICOLON'       # ;
    COMMA = 'COMMA'               # ,
    EOF = 'EOF'                   # End Of File

# Structure representing a Token produced by the lexer
class Token:
    def __init__(self, type, lexeme, line):
        self.type = type
        self.lexeme = lexeme
        self.line = line

    def __str__(self):
        return f"Token(Type: {self.type}, Lexeme: '{self.lexeme}', Line: {self.line})"

# Lexer class
class Lexer:
    def __init__(self, source_code):
        self.source_code = source_code
        self.current_pos = 0
        self.current_line = 1
        self.tokens = [] # List to store generated tokens

        self.keywords = {
            "int": TokenType.INT,
            "print": TokenType.PRINT,
            "if": TokenType.IF,
            "else": TokenType.ELSE,
            "while": TokenType.WHILE,
            "logic": TokenType.LOGIC,
            "float": TokenType.FLOAT,
            "func": TokenType.FUNC,
            "return": TokenType.RETURN,
            "true": TokenType.TRUE,
            "false": TokenType.FALSE,
        }

    def is_at_end(self):
        """Check if the lexer has reached the end of the code."""
        return self.current_pos >= len(self.source_code)

    def advance(self):
        """Get the current character and advance the position."""
        if self.is_at_end():
            return '\0'
        char = self.source_code[self.current_pos]
        self.current_pos += 1
        return char

    def peek(self):
        """Look at the current character without advancing the position."""
        if self.is_at_end():
            return '\0'
        return self.source_code[self.current_pos]

    def peek_next(self):
        """Look at the next character after the current one."""
        if self.current_pos + 1 >= len(self.source_code):
            return '\0'
        return self.source_code[self.current_pos + 1]

    def skip_whitespace(self):
        """Skip whitespace, newlines, and comments."""
        while not self.is_at_end():
            char = self.peek()
            if char.isspace():
                if char == '\n':
                    self.current_line += 1
                self.advance()
            elif char == '/' and self.peek_next() == '/': # C++ style comment: //
                while not self.is_at_end() and self.peek() != '\n':
                    self.advance()
            elif char == '/' and self.peek_next() == '*': # C/Java style comment: /* ... */
                self.advance() # Consume '/'
                self.advance() # Consume '*'
                while not self.is_at_end():
                    if self.peek() == '*' and self.peek_next() == '/':
                        self.advance() # Consume '*'
                        self.advance() # Consume '/'
                        break
                    if self.peek() == '\n':
                        self.current_line += 1
                    self.advance()
            else:
                break

    def number_token(self):
        """Create a number token (integer or float)."""
        start = self.current_pos
        is_float = False
        while self.peek().isdigit():
            self.advance()

        if self.peek() == '.' and self.peek_next().isdigit():
            is_float = True
            self.advance() # Consume '.'
            while self.peek().isdigit():
                self.advance()
        
        num_str = self.source_code[start:self.current_pos]
        return Token(TokenType.NUMBER_FLOAT if is_float else TokenType.NUMBER, num_str, self.current_line)

    def identifier_or_keyword_token(self):
        """Create an identifier or keyword token."""
        start = self.current_pos
        while self.peek().isalnum() or self.peek() == '_':
            self.advance()
        
        text = self.source_code[start:self.current_pos]
        token_type = self.keywords.get(text, TokenType.IDENTIFIER)
        return Token(token_type, text, self.current_line)

    def string_token(self):
        """Create a string literal token."""
        start_line = self.current_line
        self.advance() # Consume opening quote '"'
        start = self.current_pos
        
        while not self.is_at_end() and self.peek() != '"':
            if self.peek() == '\n':
                raise RuntimeError(f"Lexing Error: Unterminated string literal at line {start_line}")
            self.advance()

        if self.is_at_end():
            raise RuntimeError(f"Lexing Error: Unterminated string literal at line {start_line}")

        str_val = self.source_code[start:self.current_pos]
        self.advance() # Consume closing quote '"'
        return Token(TokenType.STRING, str_val, start_line)

    def get_next_token(self):
        """Function to get the next token from the source code."""
        self.skip_whitespace()

        if self.is_at_end():
            return Token(TokenType.EOF, "", self.current_line)

        char = self.peek()

        if char.isdigit():
            return self.number_token()
        if char.isalpha() or char == '_':
            return self.identifier_or_keyword_token()
        if char == '"':
            return self.string_token()

        # Handle multi-character operators first (==, !=, <=, >=)
        if char == '=':
            if self.peek_next() == '=':
                self.advance(); self.advance(); return Token(TokenType.EQUALEQUAL, "==", self.current_line)
            else:
                self.advance(); return Token(TokenType.ASSIGN, "=", self.current_line)
        if char == '!':
            if self.peek_next() == '=':
                self.advance(); self.advance(); return Token(TokenType.NOT_EQUAL, "!=", self.current_line)
            else:
                raise RuntimeError(f"Lexing Error: Unexpected character '!' at line {self.current_line}")
        if char == '<':
            if self.peek_next() == '=':
                self.advance(); self.advance(); return Token(TokenType.LESS_EQUAL, "<=", self.current_line)
            else:
                self.advance(); return Token(TokenType.LESS, "<", self.current_line)
        if char == '>':
            if self.peek_next() == '=':
                self.advance(); self.advance(); return Token(TokenType.GREATER_EQUAL, ">=", self.current_line)
            else:
                self.advance(); return Token(TokenType.GREATER, ">", self.current_line)

        # Handle single-character operators and delimiters
        if char == '+': self.advance(); return Token(TokenType.PLUS, "+", self.current_line)
        if char == '-': self.advance(); return Token(TokenType.MINUS, "-", self.current_line)
        if char == '*': self.advance(); return Token(TokenType.MULTIPLY, "*", self.current_line)
        if char == '/': self.advance(); return Token(TokenType.DIVIDE, "/", self.current_line)
        if char == '(': self.advance(); return Token(TokenType.PAREN_OPEN, "(", self.current_line)
        if char == ')': self.advance(); return Token(TokenType.PAREN_CLOSE, ")", self.current_line)
        if char == '{': self.advance(); return Token(TokenType.BRACE_OPEN, "{", self.current_line)
        if char == '}': self.advance(); return Token(TokenType.BRACE_CLOSE, "}", self.current_line)
        if char == ';': self.advance(); return Token(TokenType.SEMICOLON, ";", self.current_line)
        if char == ',': self.advance(); return Token(TokenType.COMMA, ",", self.current_line)

        raise RuntimeError(f"Lexing Error: Unexpected character '{char}' at line {self.current_line}")

# 2. Abstract Syntax Tree (AST)
#    The AST represents the hierarchical structure of the program, showing the relationships
#    between different parts of the code. Each node in the tree represents a linguistic construct.

class ASTNode:
    def compile(self, compiler):
        """Virtual method for compiling (generating Python code)."""
        raise NotImplementedError

class Statement(ASTNode):
    pass

class Expression(ASTNode):
    def compile_lvalue(self, compiler):
        """For assignments, we need the "l-value" (the variable name)."""
        raise RuntimeError("Cannot compile l-value for this expression type.")

class NumberLiteral(Expression):
    def __init__(self, value):
        self.value = value
    
    def compile(self, compiler):
        return str(self.value)

class FloatLiteral(Expression):
    def __init__(self, value):
        self.value = value
    
    def compile(self, compiler):
        return str(self.value)

class BooleanLiteral(Expression):
    def __init__(self, value):
        self.value = value
    
    def compile(self, compiler):
        return "True" if self.value else "False"

class StringLiteral(Expression):
    def __init__(self, value):
        self.value = value
    
    def compile(self, compiler):
        return f'"{self.value}"' # Python string literal

class IdentifierExpr(Expression):
    def __init__(self, name):
        self.name = name
    
    def compile(self, compiler):
        return self.name

    def compile_lvalue(self, compiler):
        return self.name

class CallExpression(Expression):
    def __init__(self, function_name, arguments):
        self.function_name = function_name
        self.arguments = arguments
    
    def compile(self, compiler):
        arg_str = ", ".join(arg.compile(compiler) for arg in self.arguments)
        return f"{self.function_name}({arg_str})"

class BinaryExpression(Expression):
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right
    
    def compile(self, compiler):
        left_py = self.left.compile(compiler)
        right_py = self.right.compile(compiler)

        if self.op == TokenType.ASSIGN:
            return f"{self.left.compile_lvalue(compiler)} = {right_py}"
        elif self.op == TokenType.PLUS:
            return f"({left_py} + {right_py})"
        elif self.op == TokenType.MINUS:
            return f"({left_py} - {right_py})"
        elif self.op == TokenType.MULTIPLY:
            return f"({left_py} * {right_py})"
        elif self.op == TokenType.DIVIDE:
            return f"({left_py} / {right_py})" # Python's / is float division
        elif self.op == TokenType.EQUALEQUAL:
            return f"({left_py} == {right_py})"
        elif self.op == TokenType.NOT_EQUAL:
            return f"({left_py} != {right_py})"
        elif self.op == TokenType.LESS:
            return f"({left_py} < {right_py})"
        elif self.op == TokenType.GREATER:
            return f"({left_py} > {right_py})"
        elif self.op == TokenType.LESS_EQUAL:
            return f"({left_py} <= {right_py})"
        elif self.op == TokenType.GREATER_EQUAL:
            return f"({left_py} >= {right_py})"
        else:
            raise RuntimeError(f"Compiler Error: Unsupported binary operator '{self.op}' for Python generation.")

class VariableDeclaration(Statement):
    def __init__(self, type, var_name, initializer=None):
        self.type = type # Not strictly used in Python output but good for type analysis in a full compiler
        self.var_name = var_name
        self.initializer = initializer
    
    def compile(self, compiler):
        if self.initializer:
            return f"{self.var_name} = {self.initializer.compile(compiler)}"
        else:
            # Python doesn't need explicit type declaration, just assignment
            # Default initialization (e.g., to 0, 0.0, False, "")
            if self.type == TokenType.INT: return f"{self.var_name} = 0"
            elif self.type == TokenType.FLOAT: return f"{self.var_name} = 0.0"
            elif self.type == TokenType.LOGIC: return f"{self.var_name} = False"
            else: return f"{self.var_name} = None" # Generic default for other types

class PrintStatement(Statement):
    def __init__(self, expr):
        self.expr = expr
    
    def compile(self, compiler):
        return f"print({self.expr.compile(compiler)})"

class ExpressionStatement(Statement):
    def __init__(self, expr):
        self.expr = expr
    
    def compile(self, compiler):
        return self.expr.compile(compiler) # Expression will naturally be a statement

class IfStatement(Statement):
    def __init__(self, condition, then_branch, else_branch=None):
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch
    
    def compile(self, compiler):
        ss = []
        ss.append(f"if {self.condition.compile(compiler)}:")
        ss.append(compiler.indent_code(self.then_branch.compile(compiler), 1))
        if self.else_branch:
            ss.append("else:")
            ss.append(compiler.indent_code(self.else_branch.compile(compiler), 1))
        return "\n".join(ss)

class WhileStatement(Statement):
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body
    
    def compile(self, compiler):
        ss = []
        ss.append(f"while {self.condition.compile(compiler)}:")
        ss.append(compiler.indent_code(self.body.compile(compiler), 1))
        return "\n".join(ss)

class FunctionDeclaration(Statement):
    def __init__(self, name, parameters, body):
        self.name = name
        self.parameters = parameters
        self.body = body
    
    def compile(self, compiler):
        param_str = ", ".join(self.parameters)
        ss = []
        ss.append(f"def {self.name}({param_str}):")
        ss.append(compiler.indent_code(self.body.compile(compiler), 1))
        return "\n".join(ss) + "\n\n" # Add extra newlines for function separation

class ReturnStatement(Statement):
    def __init__(self, expr):
        self.expr = expr
    
    def compile(self, compiler):
        return f"return {self.expr.compile(compiler)}"

class BlockStatement(Statement):
    def __init__(self):
        self.statements = []
    
    def add_statement(self, stmt):
        self.statements.append(stmt)
    
    def compile(self, compiler):
        return "\n".join(stmt.compile(compiler) for stmt in self.statements)

# 3. Syntactic Analysis (Parsing)
#    This phase takes tokens from the lexer and builds an AST according to the language's grammar rules.

class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = None
        self.advance() # Get the first token

    def advance(self):
        """Advance the current token to the next token from the lexer."""
        self.current_token = self.lexer.get_next_token()

    def check(self, type):
        """Check if the current token is of the expected type."""
        return self.current_token.type == type

    def consume(self, type, message):
        """Consume the current token if it's of the expected type, otherwise raise an error."""
        if self.check(type):
            token = self.current_token
            self.advance()
            return token
        raise RuntimeError(f"Parsing Error at line {self.current_token.line}: {message}. Got '{self.current_token.lexeme}'")

    def parse_program(self):
        """Function to start the parsing process and build the complete AST (main program block)."""
        program_block = BlockStatement() # Corrected: no 'auto'
        while not self.check(TokenType.EOF):
            program_block.add_statement(self.parse_statement())
        return program_block

    def parse(self):
        """Public entry point for parsing."""
        return self.parse_program()

    def parse_statement(self):
        """Parse a statement."""
        if self.check(TokenType.INT) or self.check(TokenType.LOGIC) or self.check(TokenType.FLOAT): # New types
            return self.parse_variable_declaration()
        elif self.check(TokenType.PRINT):
            return self.parse_print_statement()
        elif self.check(TokenType.IF):
            return self.parse_if_statement()
        elif self.check(TokenType.WHILE):
            return self.parse_while_statement()
        elif self.check(TokenType.BRACE_OPEN):
            return self.parse_block_statement()
        elif self.check(TokenType.FUNC): # New: Function declaration
            return self.parse_function_declaration()
        elif self.check(TokenType.RETURN): # New: Return statement
            return self.parse_return_statement()
        
        # Handle general expression statements (e.g., assignments, function calls)
        expr = self.parse_expression()
        self.consume(TokenType.SEMICOLON, "Expected ';' after expression statement.")
        return ExpressionStatement(expr)

    def parse_variable_declaration(self):
        """Parse a variable declaration (e.g., int x = 10; logic flag; float pi;)."""
        type_token = self.current_token.type # Store the type keyword
        if not (self.check(TokenType.INT) or self.check(TokenType.LOGIC) or self.check(TokenType.FLOAT)):
             raise RuntimeError(f"Parsing Error at line {self.current_token.line}: Expected 'int', 'logic', or 'float' keyword.")
        self.advance() # Consume the type keyword

        identifier = self.consume(TokenType.IDENTIFIER, "Expected identifier after type keyword.")

        initializer = None
        if self.check(TokenType.ASSIGN): # If there's an initializer (e.g., = 10)
            self.advance() # Consume '='
            initializer = self.parse_expression() # Parse the expression for the initial value
        
        self.consume(TokenType.SEMICOLON, "Expected ';' after variable declaration.")
        return VariableDeclaration(type_token, identifier.lexeme, initializer)

    def parse_print_statement(self):
        """Parse a print statement (e.g., print(x + 5);)."""
        self.consume(TokenType.PRINT, "Expected 'print' keyword.")
        self.consume(TokenType.PAREN_OPEN, "Expected '(' after 'print'.")
        expr_to_print = self.parse_expression()
        self.consume(TokenType.PAREN_CLOSE, "Expected ')' after print expression.")
        self.consume(TokenType.SEMICOLON, "Expected ';' after print statement.")
        return PrintStatement(expr_to_print)

    def parse_if_statement(self):
        """Parse an if-else statement."""
        self.consume(TokenType.IF, "Expected 'if' keyword.")
        self.consume(TokenType.PAREN_OPEN, "Expected '(' after 'if'.")
        condition = self.parse_expression()
        self.consume(TokenType.PAREN_CLOSE, "Expected ')' after if condition.")

        then_branch = self.parse_statement() # Can be a block or single statement
        else_branch = None

        if self.check(TokenType.ELSE):
            self.advance() # Consume 'else'
            else_branch = self.parse_statement() # Can be a block or single statement
        return IfStatement(condition, then_branch, else_branch)

    def parse_while_statement(self):
        """Parse a while loop."""
        self.consume(TokenType.WHILE, "Expected 'while' keyword.")
        self.consume(TokenType.PAREN_OPEN, "Expected '(' after 'while'.")
        condition = self.parse_expression()
        self.consume(TokenType.PAREN_CLOSE, "Expected ')' after while condition.")

        body = self.parse_statement() # Can be a block or single statement
        return WhileStatement(condition, body)

    def parse_function_declaration(self):
        """Parse a function declaration: func name(param1, param2) { ... }."""
        self.consume(TokenType.FUNC, "Expected 'func' keyword.")
        func_name = self.consume(TokenType.IDENTIFIER, "Expected function name after 'func'.")
        self.consume(TokenType.PAREN_OPEN, "Expected '(' after function name.")

        parameters = []
        if not self.check(TokenType.PAREN_CLOSE): # Check if there are parameters
            while True:
                param_name = self.consume(TokenType.IDENTIFIER, "Expected parameter name or ')' in function declaration.")
                parameters.append(param_name.lexeme)
                if not self.check(TokenType.COMMA):
                    break
                self.advance() # Consume comma
        self.consume(TokenType.PAREN_CLOSE, "Expected ')' after function parameters.")

        body = self.parse_block_statement() # Function body must be a block
        return FunctionDeclaration(func_name.lexeme, parameters, body)

    def parse_return_statement(self):
        """Parse a return statement: return expr;."""
        self.consume(TokenType.RETURN, "Expected 'return' keyword.")
        expr_to_return = self.parse_expression()
        self.consume(TokenType.SEMICOLON, "Expected ';' after return expression.")
        return ReturnStatement(expr_to_return)

    def parse_block_statement(self):
        """Parse a block statement ({ ... })."""
        self.consume(TokenType.BRACE_OPEN, "Expected '{' to start a block.")
        block = BlockStatement()
        while not self.check(TokenType.BRACE_CLOSE) and not self.check(TokenType.EOF):
            block.add_statement(self.parse_statement())
        self.consume(TokenType.BRACE_CLOSE, "Expected '}' to close a block.")
        return block

    def parse_expression(self):
        """Parse an expression - considering operator precedence."""
        # Assignment has the lowest precedence and is right-associative.
        return self.parse_assignment_expression()

    def parse_assignment_expression(self):
        """Parse assignment expressions (right-associative, lowest precedence)."""
        expr = self.parse_comparison_expression() # Start with higher precedence
        if self.check(TokenType.ASSIGN):
            assign_op = self.current_token
            self.advance() # Consume '='
            right_expr = self.parse_assignment_expression() # Right-associative
            expr = BinaryExpression(expr, assign_op.type, right_expr)
        return expr

    def parse_comparison_expression(self):
        """Parse comparison expressions (==, !=, <, >, <=, >=)."""
        expr = self.parse_additive_expression() # Start with higher precedence
        while self.check(TokenType.EQUALEQUAL) or self.check(TokenType.NOT_EQUAL) or \
              self.check(TokenType.LESS) or self.check(TokenType.GREATER) or \
              self.check(TokenType.LESS_EQUAL) or self.check(TokenType.GREATER_EQUAL):
            op_token = self.current_token
            self.advance() # Consume operator
            right_expr = self.parse_additive_expression()
            expr = BinaryExpression(expr, op_token.type, right_expr)
        return expr

    def parse_additive_expression(self):
        """Parse additive expressions (+, -)."""
        expr = self.parse_multiplicative_expression() # Start with higher precedence
        while self.check(TokenType.PLUS) or self.check(TokenType.MINUS):
            op_token = self.current_token
            self.advance() # Consume operator
            right_expr = self.parse_multiplicative_expression()
            expr = BinaryExpression(expr, op_token.type, right_expr)
        return expr

    def parse_multiplicative_expression(self):
        """Parse multiplicative expressions (*, /)."""
        expr = self.parse_primary_expression() # Start with highest precedence
        while self.check(TokenType.MULTIPLY) or self.check(TokenType.DIVIDE):
            op_token = self.current_token
            self.advance() # Consume operator
            right_expr = self.parse_primary_expression()
            expr = BinaryExpression(expr, op_token.type, right_expr)
        return expr

    def parse_primary_expression(self):
        """Parse primary expressions like numbers, identifiers, expressions in parentheses, or string literals."""
        if self.check(TokenType.NUMBER):
            num_token = self.consume(TokenType.NUMBER, "Expected an integer number.")
            return NumberLiteral(int(num_token.lexeme))
        elif self.check(TokenType.NUMBER_FLOAT):
            float_token = self.consume(TokenType.NUMBER_FLOAT, "Expected a floating-point number.")
            return FloatLiteral(float(float_token.lexeme))
        elif self.check(TokenType.STRING):
            str_token = self.consume(TokenType.STRING, "Expected a string literal.")
            return StringLiteral(str_token.lexeme)
        elif self.check(TokenType.TRUE):
            self.advance()
            return BooleanLiteral(True)
        elif self.check(TokenType.FALSE):
            self.advance()
            return BooleanLiteral(False)
        elif self.check(TokenType.IDENTIFIER):
            id_token = self.consume(TokenType.IDENTIFIER, "Expected an identifier or function call.")
            if self.check(TokenType.PAREN_OPEN): # This is a function call
                self.advance() # Consume '('
                arguments = []
                if not self.check(TokenType.PAREN_CLOSE): # Check for arguments
                    while True:
                        arguments.append(self.parse_expression())
                        if not self.check(TokenType.COMMA):
                            break
                        self.advance() # Consume comma
                self.consume(TokenType.PAREN_CLOSE, "Expected ')' after function arguments.")
                return CallExpression(id_token.lexeme, arguments)
            # Otherwise, it's just an identifier (variable access)
            return IdentifierExpr(id_token.lexeme)
        elif self.check(TokenType.PAREN_OPEN):
            self.advance() # Consume '('
            expr = self.parse_expression() # Parse the expression inside parentheses
            self.consume(TokenType.PAREN_CLOSE, "Expected ')' after expression.")
            return expr
        raise RuntimeError(f"Parsing Error at line {self.current_token.line}: Expected number, float, string, boolean, identifier, or '(' in expression. Got '{self.current_token.lexeme}'")

# 4. Compiler - Python code generation part
#    This class orchestrates the compilation process, generating Python code from the AST.

class Compiler:
    def __init__(self, program_ast):
        self.root = program_ast
        self.declared_functions = set() # To keep track of function names for validation/calls
        # Populate declared_functions set during compilation setup
        for statement in self.root.statements:
            if isinstance(statement, FunctionDeclaration):
                self.declared_functions.add(statement.name)

    def indent_code(self, code_str, level):
        """Helper to add indentation to generated Python code for blocks."""
        indentation = ' ' * (level * 4) # 4 spaces per level
        lines = code_str.splitlines()
        indented_lines = []
        for line in lines:
            if line.strip(): # Only indent non-empty lines
                indented_lines.append(indentation + line)
            else:
                indented_lines.append(line) # Preserve empty lines
        return "\n".join(indented_lines)

    def compile_program(self):
        """Generates the full Python code."""
        python_code_stream = []

        # Add initial comments or header if needed
        python_code_stream.append("# Generated by RACe Compiler to Python\n")

        # Compile all statements in the root block
        for statement in self.root.statements:
            python_code_stream.append(statement.compile(self))
        
        # In Python, we don't need a specific main function wrapper
        # The code will execute top-to-bottom.
        # However, for organization, we could add a standard `if __name__ == "__main__":` block.
        # For simplicity, we'll just output the compiled statements directly.

        return "\n".join(python_code_stream)


# Main function to run the compiler
def main():
    if len(sys.argv) != 2:
        print(f"Usage: python {sys.argv[0]} <rac_file_path>", file=sys.stderr)
        sys.exit(1)

    file_path = sys.argv[1]
    
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            race_code = f.read()
    except FileNotFoundError:
        print(f"Error: Could not open file '{file_path}'", file=sys.stderr)
        sys.exit(1)

    print(f"--- Compiling RACe Source Code from '{file_path}' ---")
    print(race_code)
    print("--------------------------------------------------------\n")

    try:
        # Lexical Analysis (Lexing) phase
        lexer = Lexer(race_code)
        
        # Syntactic Analysis (Parsing) phase
        parser = Parser(lexer)
        program_ast = parser.parse() # Now calls the new public parse method
        print("Parsing successful! AST built.\n")

        # Compilation phase (generating Python code)
        compiler = Compiler(program_ast)
        python_output = compiler.compile_program()

        print("--- Generated Python Code ---")
        print(python_output)
        print("-----------------------------\n")

        print("To run the generated Python code, save it to a file (e.g., 'output.py') and use:\n")
        print("python output.py\n")

    except RuntimeError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()

