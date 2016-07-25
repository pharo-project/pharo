opcode sizing
sizeTrapIfNotInstanceOf: litIndex 
	^ self sizeOpcodeSelector: #genTrapIfNotInstanceOf: withArguments: {litIndex}
