
public class RACeValue {
    public enum RACeType { INT, FLOAT, STRING, BOOLEAN }

    public RACeType type;
    public int intVal;
    public double floatVal;
    public String stringVal;
    public boolean boolVal;

    private RACeValue(int val) { this.type = RACeType.INT; this.intVal = val; }
    private RACeValue(double val) { this.type = RACeType.FLOAT; this.floatVal = val; }
    private RACeValue(String val) { this.type = RACeType.STRING; this.stringVal = val; }
    private RACeValue(boolean val) { this.type = RACeType.BOOLEAN; this.boolVal = val; }

    public static RACeValue createInt(int val) { return new RACeValue(val); }
    public static RACeValue createFloat(double val) { return new RACeValue(val); }
    public static RACeValue createString(String val) { return new RACeValue(val); }
    public static RACeValue createBool(boolean val) { return new RACeValue(val); }

    @Override
    public String toString() {
        switch (type) {
            case INT: return String.valueOf(intVal);
            case FLOAT:
                String s = String.format("%.6f", floatVal);
                if (s.contains(".")) {
                    s = s.replaceAll("0*$", "");
                    if (s.endsWith(".")) {
                        s = s.substring(0, s.length() - 1);
                    }
                }
                return s;
            case STRING: return stringVal;
            case BOOLEAN: return String.valueOf(boolVal);
            default: return "<unknown_type>";
        }
    }

    public static void print(RACeValue val) {
        System.out.println(val.toString());
    }

    public boolean isTruthy() {
        switch (type) {
            case INT: return intVal != 0;
            case FLOAT: return floatVal != 0.0;
            case STRING: return !stringVal.isEmpty();
            case BOOLEAN: return boolVal;
            default: return false;
        }
    }

    public static RACeValue add(RACeValue left, RACeValue right) {
        if (left.type == RACeType.STRING || right.type == RACeType.STRING) {
            return createString(left.toString() + right.toString());
        } else if (left.type == RACeType.FLOAT || right.type == RACeType.FLOAT) {
            double l = (left.type == RACeType.INT) ? left.intVal : left.floatVal;
            double r = (right.type == RACeType.INT) ? right.intVal : right.floatVal;
            return createFloat(l + r);
        } else if (left.type == RACeType.INT && right.type == RACeType.INT) {
            return createInt(left.intVal + right.intVal);
        }
        throw new RuntimeException("Runtime Error: Unsupported types for '+' operator.");
    }

    public static RACeValue subtract(RACeValue left, RACeValue right) {
        ensureNumericOperands("-", left, right);
        if (left.type == RACeType.FLOAT || right.type == RACeType.FLOAT) {
            double l = (left.type == RACeType.INT) ? left.intVal : left.floatVal;
            double r = (right.type == RACeType.INT) ? right.intVal : right.floatVal;
            return createFloat(l - r);
        } else {
            return createInt(left.intVal - right.intVal);
        }
    }

    public static RACeValue multiply(RACeValue left, RACeValue right) {
        ensureNumericOperands("*", left, right);
        if (left.type == RACeType.FLOAT || right.type == RACeType.FLOAT) {
            double l = (left.type == RACeType.INT) ? left.intVal : left.floatVal;
            double r = (right.type == RACeType.INT) ? right.intVal : right.floatVal;
            return createFloat(l * r);
        } else {
            return createInt(left.intVal * right.intVal);
        }
    }

    public static RACeValue divide(RACeValue left, RACeValue right) {
        ensureNumericOperands("/", left, right);
        double l = (left.type == RACeType.INT) ? left.intVal : left.floatVal;
        double r = (right.type == RACeType.INT) ? right.intVal : right.floatVal;
        if (r == 0.0) throw new RuntimeException("Runtime Error: Division by zero.");
        if (left.type == RACeType.INT && right.type == RACeType.INT && (l % r == 0.0)) {
            return createInt((int)(l / r));
        }
        return createFloat(l / r);
    }

    public static RACeValue equal(RACeValue left, RACeValue right) {
        if (left.type != right.type) return createBool(false);
        switch (left.type) {
            case INT: return createBool(left.intVal == right.intVal);
            case FLOAT: return createBool(left.floatVal == right.floatVal);
            case STRING: return createBool(left.stringVal.equals(right.stringVal));
            case BOOLEAN: return createBool(left.boolVal == right.boolVal);
            default: return createBool(false);
        }
    }

    public static RACeValue notEqual(RACeValue left, RACeValue right) {
        return createBool(!equal(left, right).boolVal);
    }

    public static RACeValue less(RACeValue left, RACeValue right) {
        ensureNumericComparison("<", left, right);
        double l = (left.type == RACeType.INT) ? left.intVal : left.floatVal;
        double r = (right.type == RACeType.INT) ? right.intVal : right.floatVal;
        return createBool(l < r);
    }

    public static RACeValue greater(RACeValue left, RACeValue right) {
        ensureNumericComparison(">", left, right);
        double l = (left.type == RACeType.INT) ? left.intVal : left.floatVal;
        double r = (right.type == RACeType.INT) ? right.intVal : right.floatVal;
        return createBool(l > r);
    }

    public static RACeValue lessEqual(RACeValue left, RACeValue right) {
        ensureNumericComparison("<=", left, right);
        double l = (left.type == RACeType.INT) ? left.intVal : left.floatVal;
        double r = (right.type == RACeType.INT) ? right.intVal : right.floatVal;
        return createBool(l <= r);
    }

    public static RACeValue greaterEqual(RACeValue left, RACeValue right) {
        ensureNumericComparison(">=", left, right);
        double l = (left.type == RACeType.INT) ? left.intVal : left.floatVal;
        double r = (right.type == RACeType.INT) ? right.intVal : right.floatVal;
        return createBool(l >= r);
    }

    private static void ensureNumericOperands(String opStr, RACeValue left, RACeValue right) {
        if (left.type == RACeType.STRING || right.type == RACeType.STRING ||
            left.type == RACeType.BOOLEAN || right.type == RACeType.BOOLEAN) {
            throw new RuntimeException("Runtime Error: Type mismatch for '" + opStr + "' operator. Both operands must be numeric (int or float).");
        }
    }

    private static void ensureNumericComparison(String opStr, RACeValue left, RACeValue right) {
        if ((left.type != RACeType.INT && left.type != RACeType.FLOAT) ||
            (right.type != RACeType.INT && right.type != RACeType.FLOAT)) {
            throw new RuntimeException("Runtime Error: Type mismatch for '" + opStr + "' operator. Both operands must be numeric.");
        }
    }
}
