opcode sizing
sizeStorePopInstVar: instVarIndex
	^self sizeOpcodeSelector: #genStorePopInstVar: withArguments: {instVarIndex}