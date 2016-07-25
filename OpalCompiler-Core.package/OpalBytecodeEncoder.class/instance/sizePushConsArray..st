opcode sizing
sizePushConsArray: numElements
	^self sizeOpcodeSelector: #genPushConsArray: withArguments: {numElements}