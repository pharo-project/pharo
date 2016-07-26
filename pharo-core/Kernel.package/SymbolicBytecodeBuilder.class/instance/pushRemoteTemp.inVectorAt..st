instruction decoding
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex 
	self addBytecode: 'pushTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString