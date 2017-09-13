opcode sizing
sizePushTemp: tempIndex
	^self sizeOpcodeSelector: #genPushTemp: withArguments: {tempIndex}