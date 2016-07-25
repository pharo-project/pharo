examples-blocks-optimized
exampleWhileTrueSameJumpTarget2
	| tmp1 tmp2 tmp3 tmp4 tmp5 tmp6 |
	"need body + triple nested loop with same start"
	tmp1 := 1.
	tmp2 := 10.
	tmp3 := 3.
	tmp4 := 5.
	tmp5 := 7.
	tmp6 := 6.
	[ 
	[ 
	[ 
	[ 
	tmp5 := tmp5 * 2.
	tmp5 < 14 ] whileTrue.
	tmp4 := tmp4 + 1.
	tmp4 < 10 ] whileTrue.
	[ 
	[ 
	tmp1 := tmp1 + 1.
	tmp1 < 10 ] whileTrue.
	tmp6 := tmp6 + 3.
	tmp6 < 20 ] whileTrue.
	tmp2 := tmp2 - 1.
	tmp2 > 0 ] whileTrue.
	tmp3 := tmp1 + 1.
	tmp1 < 10 ] whileTrue