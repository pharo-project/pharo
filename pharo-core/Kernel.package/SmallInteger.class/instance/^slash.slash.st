arithmetic
// aNumber 
	"Primitive. Divide the receiver by the argument and answer with the
	result. Round the result down towards negative infinity to make it a
	whole integer. Fail if the argument is 0 or is not a SmallInteger.
	Essential. No Lookup. See Object documentation whatIsAPrimitive. "

	<primitive: 12>
	^ super // aNumber 	"Do with quo: if primitive fails"