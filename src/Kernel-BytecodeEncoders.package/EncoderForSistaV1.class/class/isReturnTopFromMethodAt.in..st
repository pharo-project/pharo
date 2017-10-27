instruction stream support
isReturnTopFromMethodAt: pc in: method
	"Answer whether the bytecode at pc is a return stack top from method."

	^(method at: pc) = 92