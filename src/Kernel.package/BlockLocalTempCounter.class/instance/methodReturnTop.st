instruction decoding
methodReturnTop
	"Return Top Of Stack bytecode."
	stackPointer := stackPointer - 1.
	self doJoin