opcode sizing
sizeStorePopInstVarLong: instVarIndex
	^self sizeOpcodeSelector: #genStorePopInstVarLong: withArguments: {instVarIndex}