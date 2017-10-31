opcode sizing
sizeStoreInstVarLong: instVarIndex
	^self sizeOpcodeSelector: #genStoreInstVarLong: withArguments: {instVarIndex}