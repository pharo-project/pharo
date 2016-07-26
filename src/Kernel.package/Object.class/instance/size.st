accessing
size
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 62>
	self class isVariable ifFalse: [self errorNotIndexable].
	^ 0