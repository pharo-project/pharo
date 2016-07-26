arithmetic
+ anInteger 
	"Primitive. Add the receiver to the argument and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	<primitive: 21>
	^super + anInteger