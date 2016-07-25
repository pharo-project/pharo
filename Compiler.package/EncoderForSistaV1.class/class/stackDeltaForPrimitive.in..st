bytecode decoding
stackDeltaForPrimitive: primitiveIndex in: method
	"Answer the stack delta for the callPrimitive: bytecode (see my class comment).
	 There is no delta for non-inlined primitives (its implicitly 0 - method numArgs).
	 Inlined primitives are grouped by the thousand by argument count, 32 args max ;-)."
	^primitiveIndex < 32678
		ifTrue: [0]
		ifFalse: [primitiveIndex - 32768 // 1000]