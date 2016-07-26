accessing
initialPC
	"Answer the program counter for the receiver's first bytecode."

	^ (self numLiterals + 1) * Smalltalk wordSize + 1