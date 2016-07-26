instruction decoding
popIntoLiteralVariable: anAssociation 
	"Print the Remove Top Of Stack And Store Into Literal Variable bytecode."

	self addBytecode: 'popIntoLit: ' , anAssociation key