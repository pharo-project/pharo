instruction decoding
pushLiteralVariable: anAssociation
	"Print the Push Contents Of anAssociation On Top Of Stack bytecode."

	self addBytecode: 'pushLit: ' , anAssociation key