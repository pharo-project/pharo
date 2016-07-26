bit manipulation
bitOr: arg 
	"Primitive. Answer an Integer whose bits are the logical OR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 15>
	self >= 0 ifTrue: [^ arg bitOr: self].
	^ arg < 0
		ifTrue: [(self bitInvert bitAnd: arg bitInvert) bitInvert]
		ifFalse: [(self bitInvert bitClear: arg) bitInvert]