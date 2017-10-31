tests
testNumericSuffix

self assert: ('abc98' numericSuffix) = 98.
self assert: ('98abc' numericSuffix) = 0.
self assert: ('  123abc456' numericSuffix) = 456.
self assert: ('abc456 789' numericSuffix) = 789.
self assert: ('012éà ôüÖ ẞ123' numericSuffix) = 123.