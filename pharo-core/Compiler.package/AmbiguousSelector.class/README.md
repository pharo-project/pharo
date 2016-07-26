An AmbiguousSelector is a notification produced by the Scanner/Parser/Compiler when encountering this ambiguous construct:

1@-2

Upper expression can be interpreted both
1 @ -2 (regular st-80 and former Squeak syntax, the minus is attached to the literal number)
1 @- 2 (extended binary selector, the minus sign is allowed at any position and thus part of the binary selector)
