opcode sizing
sizePushLiteralVar: literalIndex
	^self sizeOpcodeSelector: #genPushLiteralVar: withArguments: {literalIndex}