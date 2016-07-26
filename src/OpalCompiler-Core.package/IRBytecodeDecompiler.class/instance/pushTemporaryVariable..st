instruction decoding
pushTemporaryVariable: offset
	irBuilder pushTemp: (self scope tempAt: offset)