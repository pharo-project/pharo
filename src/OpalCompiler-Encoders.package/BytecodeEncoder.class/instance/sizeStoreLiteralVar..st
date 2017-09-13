opcode sizing
sizeStoreLiteralVar: literalIndex
	^self sizeOpcodeSelector: #genStoreLiteralVar: withArguments: {literalIndex}