
public class Main {
    public static RACeValue calculateSum(RACeValue x, RACeValue y) {
        RACeValue s = RACeValue.add(x, y);
        RACeValue.print(RACeValue.createString("Sum inside function: "));
        RACeValue.print(s);
        return s;
    }

    public static void main(String[] args) {
        try {
            RACeValue.print(RACeValue.createString("--- Advanced RACe Features ---"));
            RACeValue pi = RACeValue.createFloat(3.141590D);
            RACeValue radius = RACeValue.createFloat(5.000000D);
            RACeValue area = RACeValue.multiply(RACeValue.multiply(pi, radius), radius);
            RACeValue.print(RACeValue.createString("Area of circle with radius "));
            RACeValue.print(radius);
            RACeValue.print(RACeValue.createString(" is: "));
            RACeValue.print(area);

            RACeValue is_raining = RACeValue.createBool(true);
            RACeValue is_sunny = RACeValue.createBool(false);
            RACeValue.print(RACeValue.createString("Is it raining? "));
            RACeValue.print(is_raining);
            RACeValue.print(RACeValue.createString("Is it sunny? "));
            RACeValue.print(is_sunny);

            if (RACeValue.equal(is_raining, RACeValue.createBool(true)).isTruthy()) {
                RACeValue.print(RACeValue.createString("Better take an umbrella!"));
            } else {
                RACeValue.print(RACeValue.createString("Enjoy the clear sky!"));
            }

            RACeValue temp = RACeValue.createInt(25);
            if (RACeValue.greater(temp, RACeValue.createInt(30)).isTruthy()) {
                RACeValue.print(RACeValue.createString("It's hot!"));
            } else {
                if (RACeValue.less(temp, RACeValue.createInt(10)).isTruthy()) {
                    RACeValue.print(RACeValue.createString("It's cold!"));
                } else {
                    RACeValue.print(RACeValue.createString("Temperature is moderate."));
                }
            }

            RACeValue countdown = RACeValue.createFloat(3.5);
            while (RACeValue.greater(countdown, RACeValue.createFloat(0.0)).isTruthy()) {
                RACeValue.print(RACeValue.createString("Countdown: "));
                RACeValue.print(countdown);
                countdown = RACeValue.subtract(countdown, RACeValue.createFloat(1.0));
            }

            RACeValue.print(RACeValue.createString("Liftoff!"));
            RACeValue.print(RACeValue.createString("--- End of RACe Program ---"));
        } catch (RuntimeException e) {
            System.err.println(e.getMessage());
        }
    }
}
