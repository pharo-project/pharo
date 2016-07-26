examples-blocks-optimized
exampleWhileTrueSameJumpTarget
	| tmp1 tmp2 |
	tmp1 := 1.
	tmp2 := 10.
	[ 
	[ 
	tmp1 := tmp1 + 1.
	tmp1 < 10 ] whileTrue.
	tmp2 := tmp2 - 1.
	tmp2 > 0 ] whileTrue