opcode sizing
sizePushLiteral: literalIndex
	^self sizeOpcodeSelector: #genPushLiteral: withArguments: {literalIndex}