opcode sizing
sizeStorePopTemp: tempIndex
	^self sizeOpcodeSelector: #genStorePopTemp: withArguments: {tempIndex}