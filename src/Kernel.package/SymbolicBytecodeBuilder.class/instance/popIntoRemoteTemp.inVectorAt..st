instruction decoding
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	self addBytecode: 'popIntoTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString