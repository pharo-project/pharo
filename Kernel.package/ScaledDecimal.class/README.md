ScaledDecimal implement a special kind of Fraction that prints in decimal notation.
It uses a limited number of digits (scale) after the decimal separation dot and round the result.
Note that a ScaledDecimal does not printOn: exactly, however it will storeOn: exactly because the full precision fraction is kept in memory.

This is mostly usefull with denominators being powers of 10.