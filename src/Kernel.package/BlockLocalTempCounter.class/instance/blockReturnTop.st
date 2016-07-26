instruction decoding
blockReturnTop
	"Return Top Of Stack bytecode."
	stackPointer := stackPointer - 1.
	scanner pc < blockEnd ifTrue:
		[self doJoin]