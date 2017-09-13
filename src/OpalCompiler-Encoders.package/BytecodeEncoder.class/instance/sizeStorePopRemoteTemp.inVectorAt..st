opcode sizing
sizeStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	^self sizeOpcodeSelector: #genStorePopRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}