instruction decoding
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize 
	| closureEnd copiedValues |
	copiedValues := irBuilder removeLast: numCopied.
	self pushScope: copiedValues numArgs: numArgs.
	closureEnd := instructionStream pc + blockSize.
	
	irBuilder
		pushClosureCopyCopiedValues: copiedValues
		args: self scope args
		jumpTo: closureEnd.