instruction decoding
storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex 
	self addBytecode: 'storeIntoTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString