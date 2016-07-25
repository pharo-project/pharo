instruction decoding
storeIntoTemporaryVariable: offset
	irBuilder storeTemp: (self scope tempAt: offset)