instruction stream support
isReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return from block."
	
	^(method at: pc) between: 120 and: 125