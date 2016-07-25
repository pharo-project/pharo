arithmetic
/ anInteger 
	"Primitive. Divide the receiver by the argument and answer with the
	result if the division is exact. Fail if the result is not a whole integer.
	Fail if the argument is 0. Fail if either the argument or the result is not
	a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive. "

	<primitive: 30>
	^super / anInteger