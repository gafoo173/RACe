#include <iostream>  // Standard input/output
#include <string>    // String manipulation
#include <vector>    // Storing tokens and AST nodes
#include <map>       // For keywords map
#include <memory>    // Smart pointers for memory management (shared_ptr)
#include <cctype>    // Character type checks (e.g., isalpha, isdigit, isspace)
#include <stdexcept> // Exception handling (std::runtime_error)
#include <sstream>   // For building C code strings
#include <fstream>   // For file input/output
#include <iomanip>   // For std::fixed and std::setprecision

// 1. Lexical Analysis (Tokenization)
//    This phase reads the code as a stream of characters and groups them into "tokens".
//    Tokens are the smallest meaningful units in the language.

// Types of tokens that the lexer can recognize
enum TokenType {
    // Keywords
    TOKEN_KEYWORD_INT,    // For integer variable declarations
    TOKEN_KEYWORD_PRINT,  // For printing values
    TOKEN_KEYWORD_IF,     // For if statements
    TOKEN_KEYWORD_ELSE,   // For else branches
    TOKEN_KEYWORD_WHILE,  // For while loops
    TOKEN_KEYWORD_LOGIC,  // For boolean type (new)
    TOKEN_KEYWORD_FLOAT,  // For float type (new)
    TOKEN_KEYWORD_FUNC,   // For function definitions (new)
    TOKEN_KEYWORD_RETURN, // For return statements (new)
    TOKEN_KEYWORD_TRUE,   // For boolean literal true (new)
    TOKEN_KEYWORD_FALSE,  // For boolean literal false (new)

    // Identifiers (variable names, function names, class names)
    TOKEN_IDENTIFIER,

    // Numbers
    TOKEN_NUMBER,           // Integer numbers
    TOKEN_NUMBER_FLOAT,     // Floating-point numbers (new)

    // String literals
    TOKEN_STRING,

    // Operators
    TOKEN_OPERATOR_ASSIGN, // =
    TOKEN_OPERATOR_PLUS,   // +
    TOKEN_OPERATOR_MINUS,  // -
    TOKEN_OPERATOR_MULTIPLY, // *
    TOKEN_OPERATOR_DIVIDE, // /

    // Comparison Operators
    TOKEN_OPERATOR_EQUALEQUAL, // ==
    TOKEN_OPERATOR_NOT_EQUAL,  // !=
    TOKEN_OPERATOR_LESS,       // <
    TOKEN_OPERATOR_GREATER,    // >
    TOKEN_OPERATOR_LESS_EQUAL, // <=
    TOKEN_OPERATOR_GREATER_EQUAL, // >=

    // Delimiters and end-of-file
    TOKEN_PAREN_OPEN,      // (
    TOKEN_PAREN_CLOSE,     // )
    TOKEN_BRACE_OPEN,      // {
    TOKEN_BRACE_CLOSE,     // }
    TOKEN_SEMICOLON,       // ;
    TOKEN_COMMA,           // , (new for function parameters)
    TOKEN_EOF              // End Of File
};

// Structure representing a Token produced by the lexer
struct Token {
    TokenType type;       // Type of the token (e.g., TOKEN_KEYWORD_INT)
    std::string lexeme;   // Textual value of the token (e.g., "int", "myVar", "123")
    int line;             // Line number where the token appears (for error reporting)

    // Helper function to print the token (for debugging)
    std::string toString() const {
        std::string type_str;
        switch (type) {
            case TOKEN_KEYWORD_INT: type_str = "INT"; break;
            case TOKEN_KEYWORD_PRINT: type_str = "PRINT"; break;
            case TOKEN_KEYWORD_IF: type_str = "IF"; break;
            case TOKEN_KEYWORD_ELSE: type_str = "ELSE"; break;
            case TOKEN_KEYWORD_WHILE: type_str = "WHILE"; break;
            case TOKEN_KEYWORD_LOGIC: type_str = "LOGIC"; break; // New
            case TOKEN_KEYWORD_FLOAT: type_str = "FLOAT"; break; // New
            case TOKEN_KEYWORD_FUNC: type_str = "FUNC"; break; // New
            case TOKEN_KEYWORD_RETURN: type_str = "RETURN"; break; // New
            case TOKEN_KEYWORD_TRUE: type_str = "TRUE"; break; // New
            case TOKEN_KEYWORD_FALSE: type_str = "FALSE"; break; // New
            case TOKEN_IDENTIFIER: type_str = "IDENTIFIER"; break;
            case TOKEN_NUMBER: type_str = "NUMBER"; break;
            case TOKEN_NUMBER_FLOAT: type_str = "NUMBER_FLOAT"; break; // New
            case TOKEN_STRING: type_str = "STRING"; break;
            case TOKEN_OPERATOR_ASSIGN: type_str = "ASSIGN (=)"; break;
            case TOKEN_OPERATOR_PLUS: type_str = "PLUS (+)"; break;
            case TOKEN_OPERATOR_MINUS: type_str = "MINUS (-)"; break;
            case TOKEN_OPERATOR_MULTIPLY: type_str = "MULTIPLY (*)"; break;
            case TOKEN_OPERATOR_DIVIDE: type_str = "DIVIDE (/)"; break;
            case TOKEN_OPERATOR_EQUALEQUAL: type_str = "EQUALEQUAL (==)"; break;
            case TOKEN_OPERATOR_NOT_EQUAL: type_str = "NOT_EQUAL (!=)"; break;
            case TOKEN_OPERATOR_LESS: type_str = "LESS (<)"; break;
            case TOKEN_OPERATOR_GREATER: type_str = "GREATER (>)"; break;
            case TOKEN_OPERATOR_LESS_EQUAL: type_str = "LESS_EQUAL (<=)"; break;
            case TOKEN_OPERATOR_GREATER_EQUAL: type_str = "GREATER_EQUAL (>=)"; break;
            case TOKEN_PAREN_OPEN: type_str = "PAREN_OPEN (()"; break;
            case TOKEN_PAREN_CLOSE: type_str = "PAREN_CLOSE ())"; break;
            case TOKEN_BRACE_OPEN: type_str = "BRACE_OPEN ({)"; break;
            case TOKEN_BRACE_CLOSE: type_str = "BRACE_CLOSE (})"; break;
            case TOKEN_SEMICOLON: type_str = "SEMICOLON (;)"; break;
            case TOKEN_COMMA: type_str = "COMMA (,)"; break; // New
            case TOKEN_EOF: type_str = "EOF"; break;
        }
        return "Token(Type: " + type_str + ", Lexeme: '" + lexeme + "', Line: " + std::to_string(line) + ")";
    }
};

// Lexer class
class Lexer {
public:
    // Lexer constructor, takes source code as a string
    Lexer(const std::string& source) :
        source_code(source),
        current_pos(0),
        current_line(1) {
        // Initialize predefined keywords
        keywords["int"] = TOKEN_KEYWORD_INT;
        keywords["print"] = TOKEN_KEYWORD_PRINT;
        keywords["if"] = TOKEN_KEYWORD_IF;
        keywords["else"] = TOKEN_KEYWORD_ELSE;
        keywords["while"] = TOKEN_KEYWORD_WHILE;
        keywords["logic"] = TOKEN_KEYWORD_LOGIC;  // New
        keywords["float"] = TOKEN_KEYWORD_FLOAT;  // New
        keywords["func"] = TOKEN_KEYWORD_FUNC;    // New
        keywords["return"] = TOKEN_KEYWORD_RETURN; // New
        keywords["true"] = TOKEN_KEYWORD_TRUE;    // New
        keywords["false"] = TOKEN_KEYWORD_FALSE;  // New
    }

    // Function to get the next token from the source code
    Token getNextToken() {
        // Ignore whitespace and newlines
        skipWhitespace();

        // If at the end of the code, return EOF token
        if (isAtEnd()) {
            return {TOKEN_EOF, "", current_line};
        }

        char c = peek(); // Look at the current character without consuming it

        // Handle numbers (integers and floats)
        if (isdigit(c)) {
            return numberToken();
        }

        // Handle identifiers or keywords
        if (isalpha(c) || c == '_') {
            return identifierOrKeywordToken();
        }

        // Handle string literals
        if (c == '"') {
            return stringToken();
        }

        // Handle multi-character operators first (==, !=, <=, >=)
        if (c == '=') {
            if (peekNext() == '=') { advance(); advance(); return {TOKEN_OPERATOR_EQUALEQUAL, "==", current_line}; }
            else { advance(); return {TOKEN_OPERATOR_ASSIGN, "=", current_line}; }
        }
        if (c == '!') {
            if (peekNext() == '=') { advance(); advance(); return {TOKEN_OPERATOR_NOT_EQUAL, "!=", current_line}; }
            else { throw std::runtime_error("Lexing Error: Unexpected character '!' at line " + std::to_string(current_line)); }
        }
        if (c == '<') {
            if (peekNext() == '=') { advance(); advance(); return {TOKEN_OPERATOR_LESS_EQUAL, "<=", current_line}; }
            else { advance(); return {TOKEN_OPERATOR_LESS, "<", current_line}; }
        }
        if (c == '>') {
            if (peekNext() == '=') { advance(); advance(); return {TOKEN_OPERATOR_GREATER_EQUAL, ">=", current_line}; }
            else { advance(); return {TOKEN_OPERATOR_GREATER, ">", current_line}; }
        }

        // Handle single-character operators and delimiters
        switch (c) {
            case '+': advance(); return {TOKEN_OPERATOR_PLUS, "+", current_line};
            case '-': advance(); return {TOKEN_OPERATOR_MINUS, "-", current_line};
            case '*': advance(); return {TOKEN_OPERATOR_MULTIPLY, "*", current_line};
            case '/': advance(); return {TOKEN_OPERATOR_DIVIDE, "/", current_line};
            case '(': advance(); return {TOKEN_PAREN_OPEN, "(", current_line};
            case ')': advance(); return {TOKEN_PAREN_CLOSE, ")", current_line};
            case '{': advance(); return {TOKEN_BRACE_OPEN, "{", current_line};
            case '}': advance(); return {TOKEN_BRACE_CLOSE, "}", current_line};
            case ';': advance(); return {TOKEN_SEMICOLON, ";", current_line};
            case ',': advance(); return {TOKEN_COMMA, ",", current_line}; // New
            default:
                // If an unknown character is encountered, throw an error
                throw std::runtime_error("Lexing Error: Unexpected character '" + std::string(1, c) + "' at line " + std::to_string(current_line));
        }
    }

private:
    std::string source_code;            // The entire source code
    int current_pos;                    // Current position of the lexer in the code
    int current_line;                   // Current line number
    std::map<std::string, TokenType> keywords; // Map for keywords and their corresponding token types

    // Check if the lexer has reached the end of the code
    bool isAtEnd() const {
        return static_cast<size_t>(current_pos) >= source_code.length(); // Fix signedness warning
    }

    // Get the current character and advance the position
    char advance() {
        if (isAtEnd()) return '\0'; // Prevent out-of-bounds access
        return source_code[current_pos++];
    }

    // Look at the current character without advancing the position
    char peek() const {
        if (isAtEnd()) return '\0';
        return source_code[current_pos];
    }

    // Look at the next character after the current one
    char peekNext() const {
        if (static_cast<size_t>(current_pos + 1) >= source_code.length()) return '\0'; // Fix signedness warning
        return source_code[current_pos + 1];
    }

    // Skip whitespace, newlines, and comments
    void skipWhitespace() {
        while (!isAtEnd()) {
            char c = peek();
            if (isspace(c)) {
                if (c == '\n') {
                    current_line++; // Increment line number on newline
                }
                advance();
            } else if (c == '/' && peekNext() == '/') { // C++ style comment: //
                while (!isAtEnd() && peek() != '\n') {
                    advance();
                }
            } else if (c == '/' && peekNext() == '*') { // C/Java style comment: /* ... */
                advance(); advance(); // Consume /*
                while (!isAtEnd() && !(peek() == '*' && peekNext() == '/')) {
                    if (peek() == '\n') current_line++;
                    advance();
                }
                if (!isAtEnd()) { // Consume */
                    advance(); advance();
                }
            }
            else {
                break; // Not whitespace or a comment, stop skipping
            }
        }
    }

    // Create a number token (integer or float)
    Token numberToken() {
        int start = current_pos;
        bool is_float = false;
        while (isdigit(peek())) {
            advance();
        }
        if (peek() == '.' && isdigit(peekNext())) { // Found a decimal point and a digit after it
            is_float = true;
            advance(); // Consume '.'
            while (isdigit(peek())) {
                advance();
            }
        }
        std::string num_str = source_code.substr(start, current_pos - start);
        return {is_float ? TOKEN_NUMBER_FLOAT : TOKEN_NUMBER, num_str, current_line};
    }

    // Create an identifier or keyword token
    Token identifierOrKeywordToken() {
        int start = current_pos;
        while (isalnum(peek()) || peek() == '_') {
            advance();
        }
        std::string text = source_code.substr(start, current_pos - start);
        // Check if the word is a predefined keyword
        if (keywords.count(text)) {
            return {keywords[text], text, current_line};
        }
        // If not a keyword, it's an identifier
        return {TOKEN_IDENTIFIER, text, current_line};
    }

    // Create a string literal token
    Token stringToken() {
        int start_line = current_line;
        advance(); // Consume opening quote '"'
        int start = current_pos;
        while (!isAtEnd() && peek() != '"') {
            if (peek() == '\n') {
                throw std::runtime_error("Lexing Error: Unterminated string literal at line " + std::to_string(start_line));
            }
            // TODO: Add handling for escaped characters here later
            advance();
        }

        if (isAtEnd()) {
            throw std::runtime_error("Lexing Error: Unterminated string literal at line " + std::to_string(start_line));
        }

        std::string str_val = source_code.substr(start, current_pos - start);
        advance(); // Consume closing quote '"'
        return {TOKEN_STRING, str_val, start_line};
    }
};

// Forward declaration of Compiler for AST nodes
class Compiler;

// 2. Abstract Syntax Tree (AST)
//    The AST represents the hierarchical structure of the program, showing the relationships
//    between different parts of the code. Each node in the tree represents a linguistic construct.

// Base class for all AST nodes
class ASTNode {
public:
    virtual ~ASTNode() = default; // Virtual destructor to ensure proper memory deallocation
    // Virtual function for compiling (generating C code), takes the compiler as a reference
    virtual std::string compile(Compiler& compiler) = 0;
};

// Base class for statements (e.g., declarations, assignment statements, print statements)
class Statement : public ASTNode {
public:
    // Statements compile to a string of C code.
    std::string compile(Compiler& compiler) override = 0;
};

// Base class for expressions (e.g., numbers, variables, arithmetic operations)
class Expression : public ASTNode {
public:
    // Expressions compile to a string of C code representing the expression's value.
    std::string compile(Compiler& compiler) override = 0;
    // For assignments, we need the "l-value" (the variable name)
    // Most expressions are not l-values, so return empty string by default.
    virtual std::string compile_lvalue(Compiler& compiler) {
        (void)compiler; // Suppress unused parameter warning explicitly if desired
        throw std::runtime_error("Cannot compile l-value for this expression type.");
    }
};

// Node to represent an integer literal (e.g., 10, 25)
class NumberLiteral : public Expression {
public:
    int value; // Integer value
    NumberLiteral(int val) : value(val) {}
    std::string compile(Compiler& compiler) override {
        (void)compiler; // Suppress unused parameter warning
        return "race_create_int(" + std::to_string(value) + ")";
    }
};

// NEW: Node to represent a float literal (e.g., 10.5, 3.14)
class FloatLiteral : public Expression {
public:
    double value; // Float value
    FloatLiteral(double val) : value(val) {}
    std::string compile(Compiler& compiler) override {
        (void)compiler; // Suppress unused parameter warning
        std::stringstream ss;
        ss << std::fixed << std::setprecision(6) << value; // Ensure full precision for C code
        return "race_create_float(" + ss.str() + ")";
    }
};

// NEW: Node to represent a boolean literal (true/false)
class BooleanLiteral : public Expression {
public:
    bool value; // Boolean value
    BooleanLiteral(bool val) : value(val) {}
    std::string compile(Compiler& compiler) override {
        (void)compiler; // Suppress unused parameter warning
        return "race_create_bool(" + std::string(value ? "true" : "false") + ")"; // Represent as C boolean literal
    }
};


// Node to represent a string literal (e.g., "Hello World")
class StringLiteral : public Expression {
public:
    std::string value; // String value
    StringLiteral(const std::string& val) : value(val) {}
    std::string compile(Compiler& compiler) override {
        (void)compiler; // Suppress unused parameter warning
        // In C, string literals are char*, so we use double quotes
        return "race_create_string(\"" + value + "\")";
    }
};

// Node to represent an identifier (e.g., myVar, count)
class IdentifierExpr : public Expression {
public:
    std::string name; // Identifier name
    IdentifierExpr(const std::string& n) : name(n) {}
    std::string compile(Compiler& compiler) override {
        (void)compiler; // Suppress unused parameter warning
        // In C, the identifier itself is the value (a RACeValue struct)
        return name;
    }
    std::string compile_lvalue(Compiler& compiler) override {
        (void)compiler; // Suppress unused parameter warning
        return name; // An identifier can be an l-value (left side of assignment)
    }
};

// Node to represent a binary expression (e.g., a + b, x = 10, a == b)
class BinaryExpression : public Expression {
public:
    std::shared_ptr<Expression> left;  // Left-hand expression
    std::shared_ptr<Expression> right; // Right-hand expression
    TokenType op;                      // Operator type (e.g., TOKEN_OPERATOR_PLUS, TOKEN_OPERATOR_ASSIGN)

    // Fix reorder warning: initialize members in declaration order
    BinaryExpression(std::shared_ptr<Expression> l, TokenType o, std::shared_ptr<Expression> r) :
        left(l), right(r), op(o) {}

    std::string compile(Compiler& compiler) override; // Forward declare to define after Compiler

};

// Node to represent a variable declaration (e.g., int x; logic flag; float pi;)
class VariableDeclaration : public Statement {
public:
    TokenType type; // TOKEN_KEYWORD_INT, TOKEN_KEYWORD_LOGIC, TOKEN_KEYWORD_FLOAT
    std::string varName;                 // Variable name
    std::shared_ptr<Expression> initializer; // Initializer expression (optional, can be nullptr)

    VariableDeclaration(TokenType t, const std::string& name, std::shared_ptr<Expression> init = nullptr) :
        type(t), varName(name), initializer(init) {}

    std::string compile(Compiler& compiler) override; // Forward declare to define after Compiler
};

// Node to represent a print statement (e.g., print(x + 5);)
class PrintStatement : public Statement {
public:
    std::shared_ptr<Expression> expr; // The expression to be printed

    PrintStatement(std::shared_ptr<Expression> e) : expr(e) {}

    std::string compile(Compiler& compiler) override; // Forward declare to define after Compiler
};

// Node to represent an expression as a statement (e.g., x = 20;)
class ExpressionStatement : public Statement {
public:
    std::shared_ptr<Expression> expr; // The expression that forms the statement

    ExpressionStatement(std::shared_ptr<Expression> e) : expr(e) {}

    std::string compile(Compiler& compiler) override; // Forward declare to define after Compiler
};

// Node for an if-else statement
class IfStatement : public Statement {
public:
    std::shared_ptr<Expression> condition;
    std::shared_ptr<Statement> then_branch; // Block or single statement
    std::shared_ptr<Statement> else_branch; // Optional else branch

    IfStatement(std::shared_ptr<Expression> cond, std::shared_ptr<Statement> then_stmt, std::shared_ptr<Statement> else_stmt = nullptr) :
        condition(cond), then_branch(then_stmt), else_branch(else_stmt) {}

    std::string compile(Compiler& compiler) override; // Forward declare to define after Compiler
};

// Node for a while loop
class WhileStatement : public Statement {
public:
    std::shared_ptr<Expression> condition;
    std::shared_ptr<Statement> body; // Block or single statement

    WhileStatement(std::shared_ptr<Expression> cond, std::shared_ptr<Statement> loop_body) :
        condition(cond), body(loop_body) {}

    std::string compile(Compiler& compiler) override; // Forward declare to define after Compiler
};

// NEW: Node for a function declaration
class FunctionDeclaration : public Statement {
public:
    std::string name;
    // Parameter types are always RACeValue in C, so we just need names
    std::vector<std::string> parameters;
    std::shared_ptr<Statement> body; // The function body (typically a BlockStatement)

    FunctionDeclaration(const std::string& n, const std::vector<std::string>& params, std::shared_ptr<Statement> b) :
        name(n), parameters(params), body(b) {}

    std::string compile(Compiler& compiler) override; // Forward declare to define after Compiler
};

// NEW: Node for a return statement
class ReturnStatement : public Statement {
public:
    std::shared_ptr<Expression> expr; // The expression to return

    ReturnStatement(std::shared_ptr<Expression> e) : expr(e) {}

    std::string compile(Compiler& compiler) override; // Forward declare to define after Compiler
};


// Node to represent a block of statements (e.g., function body or { ... } block)
class BlockStatement : public Statement {
public:
    std::vector<std::shared_ptr<Statement>> statements; // List of statements within the block

    BlockStatement() = default;

    void addStatement(std::shared_ptr<Statement> stmt) {
        statements.push_back(stmt);
    }

    std::string compile(Compiler& compiler) override; // Forward declare to define after Compiler
};


// 4. Compiler - Actual C code generation part
//    This class orchestrates the compilation process, generating C code from the AST.
class Compiler {
public:
    // Helper to add indentation to generated C code for blocks
    std::string indentCode(const std::string& code, int level) {
        std::stringstream indented_code;
        std::string indentation(level * 4, ' '); // 4 spaces per level
        std::istringstream iss(code);
        std::string line;
        while (std::getline(iss, line)) {
            if (!line.empty()) {
                indented_code << indentation << line << "\n";
            } else {
                indented_code << "\n"; // Preserve empty lines
            }
        }
        return indented_code.str();
    }

    std::shared_ptr<BlockStatement> root; // The root node of the AST (represents the entire program)

    // Constructor
    Compiler(std::shared_ptr<BlockStatement> program_ast) : root(program_ast) {}


    // Generates the full C code including helpers and main function
    std::string compileProgram() {
        std::stringstream c_code_stream;

        // C Standard Library Includes
        c_code_stream << "#include <stdio.h>\n";
        c_code_stream << "#include <stdlib.h>\n";
        c_code_stream << "#include <string.h>\n";
        c_code_stream << "#include <stdbool.h>\n"; // For bool type in C (new)
        c_code_stream << "#include <math.h>\n\n";  // For fmod, etc. (new)

        // Define RACeValue structure for dynamic typing in C
        c_code_stream << "// C structure to mimic RACe's dynamic RuntimeValue\n";
        c_code_stream << "typedef enum { RACE_INT, RACE_FLOAT, RACE_STRING, RACE_BOOL } RACeType;\n\n"; // New types
        c_code_stream << "typedef struct {\n";
        c_code_stream << "    RACeType type;\n";
        c_code_stream << "    union {\n";
        c_code_stream << "        int int_val;\n";
        c_code_stream << "        double float_val;\n"; // Use double for float (more precision)
        c_code_stream << "        char* string_val;\n";
        c_code_stream << "        bool bool_val;\n";   // New
        c_code_stream << "    } data;\n";
        c_code_stream << "} RACeValue;\n\n";

        // Helper functions for RACeValue creation
        c_code_stream << "// Helper functions for RACeValue creation\n";
        c_code_stream << "RACeValue race_create_int(int val) {\n";
        c_code_stream << "    RACeValue v;\n";
        c_code_stream << "    v.type = RACE_INT;\n";
        c_code_stream << "    v.data.int_val = val;\n";
        c_code_stream << "    return v;\n";
        c_code_stream << "}\n\n";

        c_code_stream << "RACeValue race_create_float(double val) {\n";
        c_code_stream << "    RACeValue v;\n";
        c_code_stream << "    v.type = RACE_FLOAT;\n";
        c_code_stream << "    v.data.float_val = val;\n";
        c_code_stream << "    return v;\n";
        c_code_stream << "}\n\n";

        c_code_stream << "RACeValue race_create_string(const char* val) {\n";
        c_code_stream << "    RACeValue v;\n";
        c_code_stream << "    v.type = RACE_STRING;\n";
        c_code_stream << "    // Duplicate string to own it in memory\n";
        c_code_stream << "    v.data.string_val = strdup(val);\n";
        c_code_stream << "    return v;\n";
        c_code_stream << "}\n\n";

        c_code_stream << "RACeValue race_create_bool(bool val) {\n";
        c_code_stream << "    RACeValue v;\n";
        c_code_stream << "    v.type = RACE_BOOL;\n";
        c_code_stream << "    v.data.bool_val = val;\n";
        c_code_stream << "    return v;\n";
        c_code_stream << "}\n\n";


        // Helper for converting RACeValue to string for printing/concatenation
        c_code_stream << "// Helper for converting RACeValue to string for printing/concatenation\n";
        c_code_stream << "char* race_value_to_string(RACeValue val) {\n";
        c_code_stream << "    char buffer[256]; // Max 255 digits + null for int/float, or bool string\n";
        c_code_stream << "    if (val.type == RACE_INT) {\n";
        c_code_stream << "        snprintf(buffer, sizeof(buffer), \"%d\", val.data.int_val);\n";
        c_code_stream << "        return strdup(buffer);\n";
        c_code_stream << "    } else if (val.type == RACE_FLOAT) {\n";
        c_code_stream << "        snprintf(buffer, sizeof(buffer), \"%.6f\", val.data.float_val);\n"; // Fixed precision for floats
        c_code_stream << "        // Remove trailing zeros if they make it an integer for cleaner output\n";
        c_code_stream << "        char* dot_pos = strchr(buffer, '.');\n";
        c_code_stream << "        if (dot_pos) {\n";
        c_code_stream << "            char* end = dot_pos + strlen(dot_pos) - 1;\n";
        c_code_stream << "            while (end > dot_pos && *end == '0') {\n";
        c_code_stream << "                *end-- = '\\0';\n";
        c_code_stream << "            }\n";
        c_code_stream << "            if (*end == '.') { *end = '\\0'; } // Remove dot if all zeros after it\n";
        c_code_stream << "        }\n";
        c_code_stream << "        return strdup(buffer);\n";
        c_code_stream << "    } else if (val.type == RACE_STRING) {\n";
        c_code_stream << "        return strdup(val.data.string_val);\n";
        c_code_stream << "    } else if (val.type == RACE_BOOL) {\n";
        c_code_stream << "        return strdup(val.data.bool_val ? \"true\" : \"false\");\n";
        c_code_stream << "    }\n";
        c_code_stream << "    fprintf(stderr, \"Runtime Error: Attempted to convert unknown type to string.\\n\");\n"; // Enhanced error
        c_code_stream << "    exit(1);\n"; // Exit on error
        c_code_stream << "    return strdup(\"<error>\");\n";
        c_code_stream << "}\n\n";

        // Helper for RACe's print function
        c_code_stream << "// Helper for RACe's print function\n";
        c_code_stream << "void race_print(RACeValue val) {\n";
        c_code_stream << "    char* str_val = race_value_to_string(val);\n";
        c_code_stream << "    printf(\"%s\\n\", str_val);\n";
        c_code_stream << "    free(str_val); // Free memory allocated by strdup\n";
        c_code_stream << "}\n\n";

        // Macro to ensure numeric operands for arithmetic ops
        c_code_stream << "#define ENSURE_NUMERIC_OPERANDS(op_str) \\\n";
        c_code_stream << "    if (left.type == RACE_STRING || right.type == RACE_STRING || \\\n";
        c_code_stream << "        left.type == RACE_BOOL || right.type == RACE_BOOL) { \\\n";
        c_code_stream << "        fprintf(stderr, \"Runtime Error: Type mismatch for '\" op_str \"' operator. Both operands must be numeric (int or float).\\n\"); \\\n";
        c_code_stream << "        exit(1); \\\n";
        c_code_stream << "    }\n\n";

        // Helper for RACe's addition (+) operator, handling type coercion
        c_code_stream << "// Helper for RACe's addition (+) operator, handling type coercion\n";
        c_code_stream << "RACeValue race_add(RACeValue left, RACeValue right) {\n";
        c_code_stream << "    if (left.type == RACE_STRING || right.type == RACE_STRING) {\n"; // String concatenation
        c_code_stream << "        char* left_str = race_value_to_string(left);\n";
        c_code_stream << "        char* right_str = race_value_to_string(right);\n";
        c_code_stream << "        size_t len = strlen(left_str) + strlen(right_str) + 1;\n";
        c_code_stream << "        char* result_str = (char*)malloc(len);\n";
        c_code_stream << "        if (result_str == NULL) { fprintf(stderr, \"Memory allocation failed.\\n\"); exit(1); }\n";
        c_code_stream << "        strcpy(result_str, left_str);\n";
        c_code_stream << "        strcat(result_str, right_str);\n";
        c_code_stream << "        free(left_str); // Free temporary strings\n";
        c_code_stream << "        free(right_str);\n";
        c_code_stream << "        RACeValue res = race_create_string(result_str);\n";
        c_code_stream << "        free(result_str); // Free the concatenated string after race_create_string duplicates it\n";
        c_code_stream << "        return res;\n";
        c_code_stream << "    } else if (left.type == RACE_FLOAT || right.type == RACE_FLOAT) {\n"; // Float addition
        c_code_stream << "        double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;\n";
        c_code_stream << "        double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;\n";
        c_code_stream << "        return race_create_float(l_val + r_val);\n";
        c_code_stream << "    } else if (left.type == RACE_INT && right.type == RACE_INT) {\n"; // Integer addition
        c_code_stream << "        return race_create_int(left.data.int_val + right.data.int_val);\n";
        c_code_stream << "    }\n";
        c_code_stream << "    fprintf(stderr, \"Runtime Error: Unsupported types for '+' operator.\\n\");\n";
        c_code_stream << "    exit(1);\n";
        c_code_stream << "}\n\n";

        // Helper for subtraction (-)
        c_code_stream << "// Helper for RACe's subtraction (-) operator\n";
        c_code_stream << "RACeValue race_subtract(RACeValue left, RACeValue right) {\n";
        c_code_stream << "    ENSURE_NUMERIC_OPERANDS(\"-\");\n";
        c_code_stream << "    if (left.type == RACE_FLOAT || right.type == RACE_FLOAT) {\n";
        c_code_stream << "        double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;\n";
        c_code_stream << "        double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;\n";
        c_code_stream << "        return race_create_float(l_val - r_val);\n";
        c_code_stream << "    } else {\n";
        c_code_stream << "        return race_create_int(left.data.int_val - right.data.int_val);\n";
        c_code_stream << "    }\n";
        c_code_stream << "}\n\n";

        // Helper for multiplication (*)
        c_code_stream << "// Helper for RACe's multiplication (*) operator\n";
        c_code_stream << "RACeValue race_multiply(RACeValue left, RACeValue right) {\n";
        c_code_stream << "    ENSURE_NUMERIC_OPERANDS(\"*\");\n";
        c_code_stream << "    if (left.type == RACE_FLOAT || right.type == RACE_FLOAT) {\n";
        c_code_stream << "        double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;\n";
        c_code_stream << "        double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;\n";
        c_code_stream << "        return race_create_float(l_val * r_val);\n";
        c_code_stream << "    } else {\n";
        c_code_stream << "        return race_create_int(left.data.int_val * right.data.int_val);\n";
        c_code_stream << "    }\n";
        c_code_stream << "}\n\n";

        // Helper for division (/)
        c_code_stream << "// Helper for RACe's division (/) operator\n";
        c_code_stream << "RACeValue race_divide(RACeValue left, RACeValue right) {\n";
        c_code_stream << "    ENSURE_NUMERIC_OPERANDS(\"/\");\n";
        c_code_stream << "    double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;\n";
        c_code_stream << "    double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;\n";
        c_code_stream << "    if (r_val == 0.0) {\n";
        c_code_stream << "        fprintf(stderr, \"Runtime Error: Division by zero.\\n\");\n";
        c_code_stream << "        exit(1);\n";
        c_code_stream << "    }\n";
        c_code_stream << "    // Perform float division unless both are integers and remainder is zero\n";
        c_code_stream << "    if (left.type == RACE_INT && right.type == RACE_INT && fmod(l_val, r_val) == 0.0) {\n";
        c_code_stream << "        return race_create_int(left.data.int_val / right.data.int_val);\n";
        c_code_stream << "    }\n";
        c_code_stream << "    return race_create_float(l_val / r_val);\n";
        c_code_stream << "}\n\n";

        // Helper for comparison (==, !=, <, >, <=, >=)
        c_code_stream << "// Helper for RACe's comparison operators (returns a boolean as an int RACeValue)\n";
        c_code_stream << "RACeValue race_equal(RACeValue left, RACeValue right) {\n";
        c_code_stream << "    if (left.type != right.type) return race_create_bool(false); // Different types are not equal\n";
        c_code_stream << "    if (left.type == RACE_INT) return race_create_bool(left.data.int_val == right.data.int_val);\n";
        c_code_stream << "    if (left.type == RACE_FLOAT) return race_create_bool(left.data.float_val == right.data.float_val);\n";
        c_code_stream << "    if (left.type == RACE_STRING) return race_create_bool(strcmp(left.data.string_val, right.data.string_val) == 0);\n";
        c_code_stream << "    if (left.type == RACE_BOOL) return race_create_bool(left.data.bool_val == right.data.bool_val);\n";
        c_code_stream << "    return race_create_bool(false);\n"; // Should not happen
        c_code_stream << "}\n\n";

        c_code_stream << "RACeValue race_not_equal(RACeValue left, RACeValue right) {\n";
        c_code_stream << "    return race_create_bool(!race_equal(left, right).data.bool_val);\n";
        c_code_stream << "}\n\n";

        c_code_stream << "#define ENSURE_NUMERIC_COMPARISON(op_str) \\\n";
        c_code_stream << "    if ((left.type != RACE_INT && left.type != RACE_FLOAT) || \\\n";
        c_code_stream << "        (right.type != RACE_INT && right.type != RACE_FLOAT)) { \\\n";
        c_code_stream << "        fprintf(stderr, \"Runtime Error: Type mismatch for '\" op_str \"' operator. Both operands must be numeric.\\n\"); \\\n";
        c_code_stream << "        exit(1); \\\n";
        c_code_stream << "    }\n\n";

        c_code_stream << "RACeValue race_less(RACeValue left, RACeValue right) {\n";
        c_code_stream << "    ENSURE_NUMERIC_COMPARISON(\"<\");\n";
        c_code_stream << "    double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;\n";
        c_code_stream << "    double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;\n";
        c_code_stream << "    return race_create_bool(l_val < r_val);\n";
        c_code_stream << "}\n\n";

        c_code_stream << "RACeValue race_greater(RACeValue left, RACeValue right) {\n";
        c_code_stream << "    ENSURE_NUMERIC_COMPARISON(\">\");\n";
        c_code_stream << "    double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;\n";
        c_code_stream << "    double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;\n";
        c_code_stream << "    return race_create_bool(l_val > r_val);\n";
        c_code_stream << "}\n\n";

        c_code_stream << "RACeValue race_less_equal(RACeValue left, RACeValue right) {\n";
        c_code_stream << "    ENSURE_NUMERIC_COMPARISON(\"<=\");\n";
        c_code_stream << "    double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;\n";
        c_code_stream << "    double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;\n";
        c_code_stream << "    return race_create_bool(l_val <= r_val);\n";
        c_code_stream << "}\n\n";

        c_code_stream << "RACeValue race_greater_equal(RACeValue left, RACeValue right) {\n";
        c_code_stream << "    ENSURE_NUMERIC_COMPARISON(\">=\");\n";
        c_code_stream << "    double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;\n";
        c_code_stream << "    double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;\n";
        c_code_stream << "    return race_create_bool(l_val >= r_val);\n";
        c_code_stream << "}\n\n";

        // Helper for checking truthiness in RACe (non-zero int/float is true, non-empty string is true, bool is itself)
        c_code_stream << "// Helper for checking truthiness in RACe (non-zero int/float is true, non-empty string is true, bool is itself)\n";
        c_code_stream << "int race_is_truthy(RACeValue val) {\n";
        c_code_stream << "    if (val.type == RACE_INT) return val.data.int_val != 0;\n";
        c_code_stream << "    if (val.type == RACE_FLOAT) return val.data.float_val != 0.0;\n";
        c_code_stream << "    if (val.type == RACE_STRING) return strlen(val.data.string_val) > 0;\n";
        c_code_stream << "    if (val.type == RACE_BOOL) return val.data.bool_val;\n";
        c_code_stream << "    return 0; // Should not happen for currently supported types\n";
        c_code_stream << "}\n\n";


        // Collect function declarations and generate prototypes first
        std::stringstream function_prototypes;
        std::stringstream function_definitions;
        
        // Collect function definitions as separate C strings
        for (const auto& statement : root->statements) {
            std::shared_ptr<FunctionDeclaration> func_decl = std::dynamic_pointer_cast<FunctionDeclaration>(statement);
            if (func_decl) { // Check if the cast was successful
                // Add prototype
                function_prototypes << "RACeValue " << func_decl->name << "(";
                for (size_t i = 0; i < func_decl->parameters.size(); ++i) {
                    function_prototypes << "RACeValue " << func_decl->parameters[i];
                    if (i < func_decl->parameters.size() - 1) {
                        function_prototypes << ", ";
                    }
                }
                function_prototypes << ");\n";

                // Add definition
                function_definitions << func_decl->compile(*this);
            }
        }

        c_code_stream << function_prototypes.str() << "\n"; // Add prototypes after helpers
        c_code_stream << function_definitions.str() << "\n"; // Add function definitions

        // Main C function
        c_code_stream << "int main() {\n";
        // Compile only non-function statements into main
        for (const auto& statement : root->statements) {
            if (!std::dynamic_pointer_cast<FunctionDeclaration>(statement)) { // Check if the cast was successful
                c_code_stream << "    " << statement->compile(*this); // Indent statements in C output
            }
        }
        c_code_stream << "\n    return 0;\n";
        c_code_stream << "}\n";

        return c_code_stream.str();
    }
};

// Implement AST node compile methods after Compiler is fully defined
std::string BinaryExpression::compile(Compiler& compiler) {
    std::string left_c = left->compile(compiler);
    std::string right_c = right->compile(compiler);

    switch (op) {
        case TOKEN_OPERATOR_ASSIGN:
            return left->compile_lvalue(compiler) + " = " + right_c;
        case TOKEN_OPERATOR_PLUS:
            return "race_add(" + left_c + ", " + right_c + ")";
        case TOKEN_OPERATOR_MINUS:
            return "race_subtract(" + left_c + ", " + right_c + ")";
        case TOKEN_OPERATOR_MULTIPLY:
            return "race_multiply(" + left_c + ", " + right_c + ")";
        case TOKEN_OPERATOR_DIVIDE:
            return "race_divide(" + left_c + ", " + right_c + ")";
        case TOKEN_OPERATOR_EQUALEQUAL:
            return "race_equal(" + left_c + ", " + right_c + ")";
        case TOKEN_OPERATOR_NOT_EQUAL:
            return "race_not_equal(" + left_c + ", " + right_c + ")";
        case TOKEN_OPERATOR_LESS:
            return "race_less(" + left_c + ", " + right_c + ")";
        case TOKEN_OPERATOR_GREATER:
            return "race_greater(" + left_c + ", " + right_c + ")";
        case TOKEN_OPERATOR_LESS_EQUAL:
            return "race_less_equal(" + left_c + ", " + right_c + ")";
        case TOKEN_OPERATOR_GREATER_EQUAL:
            return "race_greater_equal(" + left_c + ", " + right_c + ")";
        default:
            throw std::runtime_error("Compiler Error: Unsupported binary operator for C generation.");
    }
}

std::string VariableDeclaration::compile(Compiler& compiler) {
    std::string c_code = "RACeValue " + varName;
    if (initializer) {
        c_code += " = " + initializer->compile(compiler);
    } else {
        // Default initialization based on RACe type
        if (type == TOKEN_KEYWORD_INT) c_code += " = race_create_int(0)";
        else if (type == TOKEN_KEYWORD_FLOAT) c_code += " = race_create_float(0.0)";
        else if (type == TOKEN_KEYWORD_LOGIC) c_code += " = race_create_bool(false)"; // Default to false for logic
        else c_code += " = race_create_int(0)"; // Fallback default
    }
    c_code += ";\n";
    return c_code;
}

std::string PrintStatement::compile(Compiler& compiler) {
    return "race_print(" + expr->compile(compiler) + ");\n";
}

std::string ExpressionStatement::compile(Compiler& compiler) {
    // Evaluate the expression and add a semicolon.
    // The result of assignment (race_create_int/string) will be discarded but the assignment happens.
    return expr->compile(compiler) + ";\n";
}

std::string IfStatement::compile(Compiler& compiler) {
    std::stringstream ss;
    ss << "if (race_is_truthy(" << condition->compile(compiler) << ")) {\n";
    // Compile then_branch, adding an indentation level for its content
    ss << compiler.indentCode(then_branch->compile(compiler), 1);
    ss << "}\n";
    if (else_branch) {
        ss << "else {\n";
        // Compile else_branch, adding an indentation level for its content
        ss << compiler.indentCode(else_branch->compile(compiler), 1);
        ss << "}\n";
    }
    return ss.str();
}

std::string WhileStatement::compile(Compiler& compiler) {
    std::stringstream ss;
    ss << "while (race_is_truthy(" << condition->compile(compiler) << ")) {\n";
    // Compile body, adding an indentation level
    ss << compiler.indentCode(body->compile(compiler), 1);
    ss << "}\n";
    return ss.str();
}

std::string FunctionDeclaration::compile(Compiler& compiler) {
    std::stringstream ss;
    // Function signature: RACeValue func_name(RACeValue param1, RACeValue param2, ...)
    ss << "RACeValue " << name << "(";
    for (size_t i = 0; i < parameters.size(); ++i) {
        ss << "RACeValue " << parameters[i];
        if (i < parameters.size() - 1) {
            ss << ", ";
        }
    }
    ss << ") {\n";
    // Compile the function body, adding an indentation level
    ss << compiler.indentCode(body->compile(compiler), 1);
    ss << "}\n\n";
    return ss.str();
}

std::string ReturnStatement::compile(Compiler& compiler) {
    return "return " + expr->compile(compiler) + ";\n";
}

std::string BlockStatement::compile(Compiler& compiler) {
    std::stringstream ss;
    // Blocks in RACe compile directly to a block in C, which handles C-level scoping
    for (const auto& stmt : statements) {
        // No extra indentation here, as the parent 'if' or 'while' will add it.
        // If this is the root block, `Compiler::compileProgram` handles the main() indentation.
        ss << stmt->compile(compiler);
    }
    return ss.str();
}


// 3. Syntactic Analysis (Parsing)
//    This phase takes tokens from the lexer and builds an AST according to the language's grammar rules.

// Parser class
class Parser {
public:
    // Parser constructor, takes a reference to the lexer
    Parser(Lexer& lexer) : lexer(lexer) {
        // Get the first token to start parsing
        advance();
    }

    // Function to start the parsing process and build the complete AST (main program block)
    std::shared_ptr<BlockStatement> parse() {
        auto program_block = std::make_shared<BlockStatement>();
        while (current_token.type != TOKEN_EOF) {
            program_block->addStatement(parseStatement());
        }
        return program_block;
    }

private:
    Lexer& lexer;      // Reference to the lexer
    Token current_token; // Current token being processed by the parser

    // Advance the current token to the next token from the lexer
    void advance() {
        current_token = lexer.getNextToken();
    }

    // Check if the current token is of the expected type
    bool check(TokenType type) const {
        return current_token.type == type;
    }

    // Consume the current token if it's of the expected type, otherwise throw an error
    Token consume(TokenType type, const std::string& message) {
        if (check(type)) {
            Token token = current_token;
            advance();
            return token;
        }
        throw std::runtime_error("Parsing Error at line " + std::to_string(current_token.line) + ": " + message + ". Got '" + current_token.lexeme + "'");
    }

    // Parse a statement
    std::shared_ptr<Statement> parseStatement() {
        if (check(TOKEN_KEYWORD_INT) || check(TOKEN_KEYWORD_LOGIC) || check(TOKEN_KEYWORD_FLOAT)) { // New types
            return parseVariableDeclaration();
        } else if (check(TOKEN_KEYWORD_PRINT)) {
            return parsePrintStatement();
        } else if (check(TOKEN_KEYWORD_IF)) {
            return parseIfStatement();
        } else if (check(TOKEN_KEYWORD_WHILE)) {
            return parseWhileStatement();
        } else if (check(TOKEN_BRACE_OPEN)) {
            return parseBlockStatement();
        } else if (check(TOKEN_KEYWORD_FUNC)) { // New: Function declaration
            return parseFunctionDeclaration();
        } else if (check(TOKEN_KEYWORD_RETURN)) { // New: Return statement
            return parseReturnStatement();
        }
        // Handle general expression statements (e.g., assignments)
        std::shared_ptr<Expression> expr = parseExpression();
        // After parsing an expression, it must be followed by a semicolon to be a statement.
        consume(TOKEN_SEMICOLON, "Expected ';' after expression statement.");
        return std::make_shared<ExpressionStatement>(expr);
    }

    // Parse a variable declaration (e.g., int x = 10; logic flag; float pi;)
    std::shared_ptr<Statement> parseVariableDeclaration() {
        TokenType type_token = current_token.type; // Store the type keyword
        if (! (check(TOKEN_KEYWORD_INT) || check(TOKEN_KEYWORD_LOGIC) || check(TOKEN_KEYWORD_FLOAT)) ) {
             throw std::runtime_error("Parsing Error at line " + std::to_string(current_token.line) + ": Expected 'int', 'logic', or 'float' keyword.");
        }
        advance(); // Consume the type keyword

        Token identifier = consume(TOKEN_IDENTIFIER, "Expected identifier after type keyword.");

        std::shared_ptr<Expression> initializer = nullptr;
        if (check(TOKEN_OPERATOR_ASSIGN)) { // If there's an initializer (e.g., = 10)
            advance(); // Consume '='
            initializer = parseExpression(); // Parse the expression for the initial value
        }

        consume(TOKEN_SEMICOLON, "Expected ';' after variable declaration.");
        return std::make_shared<VariableDeclaration>(type_token, identifier.lexeme, initializer);
    }

    // Parse a print statement (e.g., print(x + 5);)
    std::shared_ptr<Statement> parsePrintStatement() {
        consume(TOKEN_KEYWORD_PRINT, "Expected 'print' keyword.");
        consume(TOKEN_PAREN_OPEN, "Expected '(' after 'print'.");
        std::shared_ptr<Expression> expr_to_print = parseExpression();
        consume(TOKEN_PAREN_CLOSE, "Expected ')' after print expression.");
        consume(TOKEN_SEMICOLON, "Expected ';' after print statement.");
        return std::make_shared<PrintStatement>(expr_to_print);
    }

    // Parse an if-else statement
    std::shared_ptr<Statement> parseIfStatement() {
        consume(TOKEN_KEYWORD_IF, "Expected 'if' keyword.");
        consume(TOKEN_PAREN_OPEN, "Expected '(' after 'if'.");
        std::shared_ptr<Expression> condition = parseExpression();
        consume(TOKEN_PAREN_CLOSE, "Expected ')' after if condition.");

        std::shared_ptr<Statement> then_branch = parseStatement(); // Can be a block or single statement
        std::shared_ptr<Statement> else_branch = nullptr;

        if (check(TOKEN_KEYWORD_ELSE)) {
            advance(); // Consume 'else'
            else_branch = parseStatement(); // Can be a block or single statement
        }
        return std::make_shared<IfStatement>(condition, then_branch, else_branch);
    }

    // Parse a while loop
    std::shared_ptr<Statement> parseWhileStatement() {
        consume(TOKEN_KEYWORD_WHILE, "Expected 'while' keyword.");
        consume(TOKEN_PAREN_OPEN, "Expected '(' after 'while'.");
        std::shared_ptr<Expression> condition = parseExpression();
        consume(TOKEN_PAREN_CLOSE, "Expected ')' after while condition.");

        std::shared_ptr<Statement> body = parseStatement(); // Can be a block or single statement
        return std::make_shared<WhileStatement>(condition, body);
    }

    // NEW: Parse a function declaration: func name(param1, param2) { ... }
    std::shared_ptr<Statement> parseFunctionDeclaration() {
        consume(TOKEN_KEYWORD_FUNC, "Expected 'func' keyword.");
        Token func_name = consume(TOKEN_IDENTIFIER, "Expected function name after 'func'.");
        consume(TOKEN_PAREN_OPEN, "Expected '(' after function name.");

        std::vector<std::string> parameters;
        if (!check(TOKEN_PAREN_CLOSE)) { // Check if there are parameters
            do {
                Token param_name = consume(TOKEN_IDENTIFIER, "Expected parameter name or ')' in function declaration.");
                parameters.push_back(param_name.lexeme);
                // No explicit types for parameters in RACe, they are implicitly RACeValue in C
            } while (check(TOKEN_COMMA) && (advance(), true)); // Consume comma and continue if found
        }
        consume(TOKEN_PAREN_CLOSE, "Expected ')' after function parameters.");

        // Function body must be a block
        std::shared_ptr<Statement> body = parseBlockStatement();
        return std::make_shared<FunctionDeclaration>(func_name.lexeme, parameters, body);
    }

    // NEW: Parse a return statement: return expr;
    std::shared_ptr<Statement> parseReturnStatement() {
        consume(TOKEN_KEYWORD_RETURN, "Expected 'return' keyword.");
        std::shared_ptr<Expression> expr_to_return = parseExpression();
        consume(TOKEN_SEMICOLON, "Expected ';' after return expression.");
        return std::make_shared<ReturnStatement>(expr_to_return);
    }

    // Parse a block statement ({ ... })
    std::shared_ptr<Statement> parseBlockStatement() {
        consume(TOKEN_BRACE_OPEN, "Expected '{' to start a block.");
        auto block = std::make_shared<BlockStatement>();
        while (!check(TOKEN_BRACE_CLOSE) && !check(TOKEN_EOF)) {
            block->addStatement(parseStatement());
        }
        consume(TOKEN_BRACE_CLOSE, "Expected '}' to close a block.");
        return block;
    }

    // Parse an expression - considering operator precedence
    std::shared_ptr<Expression> parseExpression() {
        // Assignment has the lowest precedence and is right-associative.
        // It's parsed after all other expressions.
        return parseAssignmentExpression();
    }

    // Parse assignment expressions (right-associative, lowest precedence)
    std::shared_ptr<Expression> parseAssignmentExpression() {
        std::shared_ptr<Expression> expr = parseComparisonExpression(); // Start with higher precedence
        if (check(TOKEN_OPERATOR_ASSIGN)) {
            Token assign_op = current_token;
            advance(); // Consume '='
            std::shared_ptr<Expression> right_expr = parseAssignmentExpression(); // Right-associative
            expr = std::make_shared<BinaryExpression>(expr, assign_op.type, right_expr);
        }
        return expr;
    }

    // Parse comparison expressions (==, !=, <, >, <=, >=)
    std::shared_ptr<Expression> parseComparisonExpression() {
        std::shared_ptr<Expression> expr = parseAdditiveExpression(); // Start with higher precedence
        while (check(TOKEN_OPERATOR_EQUALEQUAL) || check(TOKEN_OPERATOR_NOT_EQUAL) ||
               check(TOKEN_OPERATOR_LESS) || check(TOKEN_OPERATOR_GREATER) ||
               check(TOKEN_OPERATOR_LESS_EQUAL) || check(TOKEN_OPERATOR_GREATER_EQUAL)) {
            Token op_token = current_token;
            advance(); // Consume operator
            std::shared_ptr<Expression> right_expr = parseAdditiveExpression();
            expr = std::make_shared<BinaryExpression>(expr, op_token.type, right_expr);
        }
        return expr;
    }

    // Parse additive expressions (+, -)
    std::shared_ptr<Expression> parseAdditiveExpression() {
        std::shared_ptr<Expression> expr = parseMultiplicativeExpression(); // Start with higher precedence

        while (check(TOKEN_OPERATOR_PLUS) || check(TOKEN_OPERATOR_MINUS)) {
            Token op_token = current_token;
            advance(); // Consume operator
            std::shared_ptr<Expression> right_expr = parseMultiplicativeExpression();
            expr = std::make_shared<BinaryExpression>(expr, op_token.type, right_expr);
        }
        return expr;
    }

    // Parse multiplicative expressions (*, /)
    std::shared_ptr<Expression> parseMultiplicativeExpression() {
        std::shared_ptr<Expression> expr = parsePrimaryExpression(); // Start with highest precedence

        while (check(TOKEN_OPERATOR_MULTIPLY) || check(TOKEN_OPERATOR_DIVIDE)) {
            Token op_token = current_token;
            advance(); // Consume operator
            std::shared_ptr<Expression> right_expr = parsePrimaryExpression();
            expr = std::make_shared<BinaryExpression>(expr, op_token.type, right_expr);
        }
        return expr;
    }

    // Parse primary expressions like numbers, identifiers, expressions in parentheses, or string literals
    // NEW: Also handles float numbers and boolean literals
    std::shared_ptr<Expression> parsePrimaryExpression() {
        if (check(TOKEN_NUMBER)) {
            Token num_token = consume(TOKEN_NUMBER, "Expected an integer number.");
            return std::make_shared<NumberLiteral>(std::stoi(num_token.lexeme));
        } else if (check(TOKEN_NUMBER_FLOAT)) { // New: Float literal
            Token float_token = consume(TOKEN_NUMBER_FLOAT, "Expected a floating-point number.");
            return std::make_shared<FloatLiteral>(std::stod(float_token.lexeme));
        } else if (check(TOKEN_STRING)) {
            Token str_token = consume(TOKEN_STRING, "Expected a string literal.");
            return std::make_shared<StringLiteral>(str_token.lexeme);
        } else if (check(TOKEN_KEYWORD_TRUE)) { // New: Boolean true
            advance();
            return std::make_shared<BooleanLiteral>(true);
        } else if (check(TOKEN_KEYWORD_FALSE)) { // New: Boolean false
            advance();
            return std::make_shared<BooleanLiteral>(false);
        } else if (check(TOKEN_IDENTIFIER)) {
            Token id_token = consume(TOKEN_IDENTIFIER, "Expected an identifier.");
            // TODO: Here we would distinguish between identifier (variable) and function call
            // For now, assume it's just an identifier (variable access).
            // Function calls will be added as a separate AST node and parsing rule.
            return std::make_shared<IdentifierExpr>(id_token.lexeme);
        } else if (check(TOKEN_PAREN_OPEN)) {
            advance(); // Consume '('
            std::shared_ptr<Expression> expr = parseExpression(); // Parse the expression inside parentheses
            consume(TOKEN_PAREN_CLOSE, "Expected ')' after expression.");
            return expr;
        }
        throw std::runtime_error("Parsing Error at line " + std::to_string(current_token.line) + ": Expected number, float, string, boolean, identifier, or '(' in expression.");
    }
};


// Main function to run the compiler
int main(int argc, char* argv[]) { // Updated to accept command-line arguments
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <rac_file_path>" << std::endl;
        return 1;
    }

    std::string file_path = argv[1];
    std::string race_code;
    std::ifstream input_file(file_path);

    if (!input_file.is_open()) {
        std::cerr << "Error: Could not open file '" << file_path << "'" << std::endl;
        return 1;
    }

    // Read file content into string
    std::stringstream buffer;
    buffer << input_file.rdbuf();
    race_code = buffer.str();
    input_file.close();

    std::cout << "--- Compiling RACe Source Code from '" << file_path << "' ---" << std::endl;
    std::cout << race_code << std::endl;
    std::cout << "--------------------------------------------------------" << std::endl << std::endl;

    try {
        // Lexical Analysis (Lexing) phase
        Lexer lexer(race_code);
        
        // Syntactic Analysis (Parsing) phase
        Parser parser(lexer);
        std::shared_ptr<BlockStatement> program_ast = parser.parse();
        std::cout << "Parsing successful! AST built." << std::endl << std::endl;

        // Compilation phase (generating C code)
        Compiler compiler(program_ast);
        std::string c_output = compiler.compileProgram();

        std::cout << "--- Generated C Code ---" << std::endl;
        std::cout << c_output << std::endl;
        std::cout << "------------------------" << std::endl << std::endl;

        std::cout << "To compile and run the generated C code, save it to a file (e.g., 'output.c') and use:\n";
        std::cout << "gcc output.c -o output_program\n";
        std::cout << "./output_program\n"; // For Linux/macOS
        std::cout << "On Windows: .\\output_program.exe\n";


    } catch (const std::runtime_error& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1; // Return error code
    }

    return 0; // Return success
}
