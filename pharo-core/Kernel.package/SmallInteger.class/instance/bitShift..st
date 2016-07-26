bit manipulation
bitShift: arg 
	"Primitive. Answer an Integer whose value is the receiver's value shifted
	left by the number of bits indicated by the argument. Negative arguments
	shift right. The receiver is interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 17>
	self >= 0 ifTrue: [^ super bitShift: arg].
	^ arg >= 0
		ifTrue: [(self negated bitShift: arg) negated]
		ifFalse: [(self bitInvert bitShift: arg) bitInvert].