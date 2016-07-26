accessing
basicAt: index put: value
	"Primitive. Assumes receiver is indexable. Store the second argument 
	value in the indexable element of the receiver indicated by index. Fail 
	if the index is not an Integer or is out of bounds. Or fail if the value is 
	not of the right type for this kind of collection. Answer the value that 
	was stored. Essential. Do not override in a subclass. See Object 
	documentation whatIsAPrimitive.

	This version of basicAt: is specifically for floats, answering the most significant
	word for index 1 and the least significant word for index 2.  This alows the VM
	to store floats in whatever order it chooses while it appears to the image that
	they are always in big-endian/PowerPC order."

	<primitive: 39>
	^super basicAt: index put: value