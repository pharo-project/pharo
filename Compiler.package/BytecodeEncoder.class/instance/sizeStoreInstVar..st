opcode sizing
sizeStoreInstVar: instVarIndex
	^self sizeOpcodeSelector: #genStoreInstVar: withArguments: {instVarIndex}