opcode sizing
sizePushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize
	^self
		sizeOpcodeSelector: #genPushClosureCopyNumCopiedValues:numArgs:jumpSize:
		withArguments: {numCopied. numArgs. jumpSize}