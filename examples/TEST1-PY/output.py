print("Hello from RACe Compiler to Python!")
num_int = 10
num_float = 3.14
is_active = True
print("Integer: ")
print(num_int)
print("Float: ")
print(num_float)
print("Boolean: ")
print(is_active)
print("Sum of int and float: ")
print((num_int + num_float))
compare_result = (num_int > num_float)
print("Is int > float? ")
print(compare_result)
if is_active:
    print("Program is active.")
else:
    print("Program is inactive.")
counter = 0
while (counter < 3):
    print("Loop: ")
    print(counter)
    counter = (counter + 1)
def calculate(a, b, c):
    result = ((a * b) + c)
    print("Result inside function: ")
    print(result)
    return result


final_value = calculate(num_int, num_float, 5.5)
print("Final result from function call: ")
print(final_value)
