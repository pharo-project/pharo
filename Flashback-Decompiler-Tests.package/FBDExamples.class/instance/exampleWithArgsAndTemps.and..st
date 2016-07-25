examples
exampleWithArgsAndTemps: arg1 and: arg2
	| tmp1 tmp2 |
	tmp1 := 1.
	tmp2 := tmp1 + arg1.
	^ { tmp2 . arg2 }