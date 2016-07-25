instruction decoding
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	self addBytecode: 'closureNumCopied: ', numCopied printString
			, ' numArgs: ', numArgs printString
			, ' bytes ', scanner pc printString
			, ' to ', (scanner pc + blockSize - 1) printString.