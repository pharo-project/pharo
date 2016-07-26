instruction decoding
popIntoLiteralVariable: anAssociation 
	"Remove Top Of Stack And Store Into Literal Variable bytecode."
	stackPointer := stackPointer - 1