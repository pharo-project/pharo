instruction decoding
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Push Closure bytecode.  Either compute the end of the block if this is
	 the block we're analysing, or skip it, adjusting the stack as appropriate."
	blockEnd
		ifNil: [blockEnd := scanner pc + blockSize]
		ifNotNil:
			[stackPointer := stackPointer - numCopied + 1.
			 scanner pc: scanner pc + blockSize]