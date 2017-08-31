opcode sizing
sizeStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	^self sizeOpcodeSelector: #genStoreRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}