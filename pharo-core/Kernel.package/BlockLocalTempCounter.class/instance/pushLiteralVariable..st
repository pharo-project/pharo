instruction decoding
pushLiteralVariable: anAssociation
	"Push Contents Of anAssociation On Top Of Stack bytecode."
	stackPointer := stackPointer + 1