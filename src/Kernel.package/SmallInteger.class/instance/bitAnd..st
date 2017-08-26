bit manipulation
bitAnd: arg 
	"Primitive. Answer an Integer whose bits are the logical AND of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 14>
	self >= 0 ifTrue: [^ arg bitAnd: self].
	^ (self bitInvert bitOr: arg bitInvert) bitInvert.