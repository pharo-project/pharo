instructions
pushFullBlockClosure: fullBlock
	fullBlock copiedValues size timesRepeat: [ stack pop ].
	stack push.
	encoder 
		genPushFullClosure: (self literalIndexOf: fullBlock compiledBlock) 
		numCopied: fullBlock copiedValues size
		receiverOnStack: false
		outerContextNeeded: true