I am InexactFloatPrintPolicy.

Through FloatPrintPolicy and double dispatch I force Float>>#printOn:base: to dynamically use the faster but potentially less accurate way to print Floats using Float>>#absPrintOn:base: