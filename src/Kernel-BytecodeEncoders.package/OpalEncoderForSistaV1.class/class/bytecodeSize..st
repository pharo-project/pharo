instruction stream support
bytecodeSize: bytecode
	"Answer the number of bytes in the bytecode."
	bytecode < 224 ifTrue: [^1].
	bytecode < 248 ifTrue: [^2].
	^3