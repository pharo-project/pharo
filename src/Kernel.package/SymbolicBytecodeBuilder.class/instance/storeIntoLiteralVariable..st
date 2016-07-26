instruction decoding
storeIntoLiteralVariable: anAssociation 
	"Print the Store Top Of Stack Into Literal Variable Of Method bytecode."

	self addBytecode: 'storeIntoLit: ' , anAssociation key