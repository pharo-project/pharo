comparing
<= anInteger 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is less than or equal to the argument. Otherwise answer false.
	Fail if the argument is not a SmallInteger or a LargePositiveInteger less
	than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 25>
	^super <= anInteger