#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

// C structure to mimic RACe's dynamic RuntimeValue
typedef enum { RACE_INT, RACE_FLOAT, RACE_STRING, RACE_BOOL } RACeType;

typedef struct {
    RACeType type;
    union {
        int int_val;
        double float_val;
        char* string_val;
        bool bool_val;
    } data;
} RACeValue;

// Helper functions for RACeValue creation
RACeValue race_create_int(int val) {
    RACeValue v;
    v.type = RACE_INT;
    v.data.int_val = val;
    return v;
}

RACeValue race_create_float(double val) {
    RACeValue v;
    v.type = RACE_FLOAT;
    v.data.float_val = val;
    return v;
}

RACeValue race_create_string(const char* val) {
    RACeValue v;
    v.type = RACE_STRING;
    // Duplicate string to own it in memory
    v.data.string_val = strdup(val);
    return v;
}

RACeValue race_create_bool(bool val) {
    RACeValue v;
    v.type = RACE_BOOL;
    v.data.bool_val = val;
    return v;
}

// Helper for converting RACeValue to string for printing/concatenation
char* race_value_to_string(RACeValue val) {
    char buffer[256]; // Max 255 digits + null for int/float, or bool string
    if (val.type == RACE_INT) {
        snprintf(buffer, sizeof(buffer), "%d", val.data.int_val);
        return strdup(buffer);
    } else if (val.type == RACE_FLOAT) {
        snprintf(buffer, sizeof(buffer), "%.6f", val.data.float_val);
        // Remove trailing zeros if they make it an integer for cleaner output
        char* dot_pos = strchr(buffer, '.');
        if (dot_pos) {
            char* end = dot_pos + strlen(dot_pos) - 1;
            while (end > dot_pos && *end == '0') {
                *end-- = '\0';
            }
            if (*end == '.') { *end = '\0'; } // Remove dot if all zeros after it
        }
        return strdup(buffer);
    } else if (val.type == RACE_STRING) {
        return strdup(val.data.string_val);
    } else if (val.type == RACE_BOOL) {
        return strdup(val.data.bool_val ? "true" : "false");
    }
    fprintf(stderr, "Runtime Error: Attempted to convert unknown type to string.\n");
    exit(1);
    return strdup("<error>");
}

// Helper for RACe's print function
void race_print(RACeValue val) {
    char* str_val = race_value_to_string(val);
    printf("%s\n", str_val);
    free(str_val); // Free memory allocated by strdup
}

#define ENSURE_NUMERIC_OPERANDS(op_str) \
    if (left.type == RACE_STRING || right.type == RACE_STRING || \
        left.type == RACE_BOOL || right.type == RACE_BOOL) { \
        fprintf(stderr, "Runtime Error: Type mismatch for '" op_str "' operator. Both operands must be numeric (int or float).\n"); \
        exit(1); \
    }

// Helper for RACe's addition (+) operator, handling type coercion
RACeValue race_add(RACeValue left, RACeValue right) {
    if (left.type == RACE_STRING || right.type == RACE_STRING) {
        char* left_str = race_value_to_string(left);
        char* right_str = race_value_to_string(right);
        size_t len = strlen(left_str) + strlen(right_str) + 1;
        char* result_str = (char*)malloc(len);
        if (result_str == NULL) { fprintf(stderr, "Memory allocation failed.\n"); exit(1); }
        strcpy(result_str, left_str);
        strcat(result_str, right_str);
        free(left_str); // Free temporary strings
        free(right_str);
        RACeValue res = race_create_string(result_str);
        free(result_str); // Free the concatenated string after race_create_string duplicates it
        return res;
    } else if (left.type == RACE_FLOAT || right.type == RACE_FLOAT) {
        double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;
        double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;
        return race_create_float(l_val + r_val);
    } else if (left.type == RACE_INT && right.type == RACE_INT) {
        return race_create_int(left.data.int_val + right.data.int_val);
    }
    fprintf(stderr, "Runtime Error: Unsupported types for '+' operator.\n");
    exit(1);
}

// Helper for RACe's subtraction (-) operator
RACeValue race_subtract(RACeValue left, RACeValue right) {
    ENSURE_NUMERIC_OPERANDS("-");
    if (left.type == RACE_FLOAT || right.type == RACE_FLOAT) {
        double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;
        double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;
        return race_create_float(l_val - r_val);
    } else {
        return race_create_int(left.data.int_val - right.data.int_val);
    }
}

// Helper for RACe's multiplication (*) operator
RACeValue race_multiply(RACeValue left, RACeValue right) {
    ENSURE_NUMERIC_OPERANDS("*");
    if (left.type == RACE_FLOAT || right.type == RACE_FLOAT) {
        double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;
        double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;
        return race_create_float(l_val * r_val);
    } else {
        return race_create_int(left.data.int_val * right.data.int_val);
    }
}

// Helper for RACe's division (/) operator
RACeValue race_divide(RACeValue left, RACeValue right) {
    ENSURE_NUMERIC_OPERANDS("/");
    double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;
    double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;
    if (r_val == 0.0) {
        fprintf(stderr, "Runtime Error: Division by zero.\n");
        exit(1);
    }
    // Perform float division unless both are integers and remainder is zero
    if (left.type == RACE_INT && right.type == RACE_INT && fmod(l_val, r_val) == 0.0) {
        return race_create_int(left.data.int_val / right.data.int_val);
    }
    return race_create_float(l_val / r_val);
}

// Helper for RACe's comparison operators (returns a boolean as an int RACeValue)
RACeValue race_equal(RACeValue left, RACeValue right) {
    if (left.type != right.type) return race_create_bool(false); // Different types are not equal
    if (left.type == RACE_INT) return race_create_bool(left.data.int_val == right.data.int_val);
    if (left.type == RACE_FLOAT) return race_create_bool(left.data.float_val == right.data.float_val);
    if (left.type == RACE_STRING) return race_create_bool(strcmp(left.data.string_val, right.data.string_val) == 0);
    if (left.type == RACE_BOOL) return race_create_bool(left.data.bool_val == right.data.bool_val);
    return race_create_bool(false);
}

RACeValue race_not_equal(RACeValue left, RACeValue right) {
    return race_create_bool(!race_equal(left, right).data.bool_val);
}

#define ENSURE_NUMERIC_COMPARISON(op_str) \
    if ((left.type != RACE_INT && left.type != RACE_FLOAT) || \
        (right.type != RACE_INT && right.type != RACE_FLOAT)) { \
        fprintf(stderr, "Runtime Error: Type mismatch for '" op_str "' operator. Both operands must be numeric.\n"); \
        exit(1); \
    }

RACeValue race_less(RACeValue left, RACeValue right) {
    ENSURE_NUMERIC_COMPARISON("<");
    double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;
    double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;
    return race_create_bool(l_val < r_val);
}

RACeValue race_greater(RACeValue left, RACeValue right) {
    ENSURE_NUMERIC_COMPARISON(">");
    double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;
    double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;
    return race_create_bool(l_val > r_val);
}

RACeValue race_less_equal(RACeValue left, RACeValue right) {
    ENSURE_NUMERIC_COMPARISON("<=");
    double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;
    double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;
    return race_create_bool(l_val <= r_val);
}

RACeValue race_greater_equal(RACeValue left, RACeValue right) {
    ENSURE_NUMERIC_COMPARISON(">=");
    double l_val = (left.type == RACE_INT) ? (double)left.data.int_val : left.data.float_val;
    double r_val = (right.type == RACE_INT) ? (double)right.data.int_val : right.data.float_val;
    return race_create_bool(l_val >= r_val);
}

// Helper for checking truthiness in RACe (non-zero int/float is true, non-empty string is true, bool is itself)
int race_is_truthy(RACeValue val) {
    if (val.type == RACE_INT) return val.data.int_val != 0;
    if (val.type == RACE_FLOAT) return val.data.float_val != 0.0;
    if (val.type == RACE_STRING) return strlen(val.data.string_val) > 0;
    if (val.type == RACE_BOOL) return val.data.bool_val;
    return 0; // Should not happen for currently supported types
}

RACeValue calculateSum(RACeValue x, RACeValue y);

RACeValue calculateSum(RACeValue x, RACeValue y) {
    RACeValue s = race_add(x, y);
    race_print(race_create_string("Sum inside function: "));
    race_print(s);
    return s;
}


int main() {
    race_print(race_create_string("--- Advanced RACe Features ---"));
    RACeValue pi = race_create_float(3.141590);
    RACeValue radius = race_create_float(5.000000);
    RACeValue area = race_multiply(race_multiply(pi, radius), radius);
    race_print(race_create_string("Area of circle with radius "));
    race_print(radius);
    race_print(race_create_string(" is: "));
    race_print(area);
    RACeValue is_raining = race_create_bool(true);
    RACeValue is_sunny = race_create_bool(false);
    race_print(race_create_string("Is it raining? "));
    race_print(is_raining);
    race_print(race_create_string("Is it sunny? "));
    race_print(is_sunny);
    if (race_is_truthy(race_equal(is_raining, race_create_bool(true)))) {
    race_print(race_create_string("Better take an umbrella!"));
}
else {
    race_print(race_create_string("Enjoy the clear sky!"));
}
    RACeValue temp = race_create_int(25);
    if (race_is_truthy(race_greater(temp, race_create_int(30)))) {
    race_print(race_create_string("It's hot!"));
}
else {
    if (race_is_truthy(race_less(temp, race_create_int(10)))) {
        race_print(race_create_string("It's cold!"));
    }
    else {
        race_print(race_create_string("Temperature is moderate."));
    }
}
    RACeValue countdown = race_create_float(3.500000);
    while (race_is_truthy(race_greater(countdown, race_create_float(0.000000)))) {
    race_print(race_create_string("Countdown: "));
    race_print(countdown);
    countdown = race_subtract(countdown, race_create_float(1.000000));
}
    race_print(race_create_string("Liftoff!"));
    race_print(race_create_string("--- End of RACe Program ---"));

    return 0;
}
