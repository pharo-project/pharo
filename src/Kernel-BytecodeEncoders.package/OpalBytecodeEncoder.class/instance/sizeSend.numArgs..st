opcode sizing
sizeSend: selectorLiteralIndex numArgs: nArgs
	^self sizeOpcodeSelector: #genSend:numArgs: withArguments: {selectorLiteralIndex. nArgs}