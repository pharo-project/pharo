opcode sizing
sizePushInstVar: instVarIndex
	^self sizeOpcodeSelector: #genPushInstVar: withArguments: {instVarIndex}