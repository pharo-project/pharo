I am ExactFloatPrintPolicy.

Through FloatPrintPolicy and double dispatch I force Float>>#printOn:base: to dynamically use the slower but accurate way to print Floats using Float>>#absPrintExactlyOn:base: