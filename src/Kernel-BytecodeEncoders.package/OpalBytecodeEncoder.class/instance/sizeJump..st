opcode sizing
sizeJump: distance
	^self sizeOpcodeSelector: #genJump: withArguments: {distance}