opcode sizing
sizeReturnSpecialLiteral: specialLiteral
	^self sizeOpcodeSelector: #genReturnSpecialLiteral: withArguments: {specialLiteral}