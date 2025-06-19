#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

// ... [Code skipped for brevity - full code includes all RACeValue helpers and logic] ...

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
    } else {
        race_print(race_create_string("Enjoy the clear sky!"));
    }
    RACeValue temp = race_create_int(25);
    if (race_is_truthy(race_greater(temp, race_create_int(30)))) {
        race_print(race_create_string("It's hot!"));
    } else {
        if (race_is_truthy(race_less(temp, race_create_int(10)))) {
            race_print(race_create_string("It's cold!"));
        } else {
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
