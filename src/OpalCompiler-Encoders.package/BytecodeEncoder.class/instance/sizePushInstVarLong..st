opcode sizing
sizePushInstVarLong: instVarIndex
	^self sizeOpcodeSelector: #genPushInstVarLong: withArguments: {instVarIndex}