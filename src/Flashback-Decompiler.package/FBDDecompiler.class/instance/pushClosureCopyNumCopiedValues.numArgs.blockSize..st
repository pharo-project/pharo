data flow instructions
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	| savedSimStack savedSequence |
	savedSimStack := simulatedStack.
	savedSequence := currentSequence.
	savedSimStack addLast: (self
		extractBlockWith: (self popFromStack: numCopied)
		numArgs: numArgs
		blockSize: blockSize).
	simulatedStack := savedSimStack.
	currentSequence := savedSequence.