bit manipulation
bitXor: arg 
	"Primitive. Answer an Integer whose bits are the logical XOR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 16>
	self >= 0 ifTrue: [^ arg bitXor: self].
	^ arg < 0
		ifTrue: [self bitInvert bitXor: arg bitInvert]
		ifFalse: [(self bitInvert bitXor: arg) bitInvert].