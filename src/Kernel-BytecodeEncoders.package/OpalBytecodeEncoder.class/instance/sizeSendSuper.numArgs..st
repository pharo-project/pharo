opcode sizing
sizeSendSuper: selectorLiteralIndex numArgs: nArgs
	^self sizeOpcodeSelector: #genSendSuper:numArgs: withArguments: {selectorLiteralIndex. nArgs}