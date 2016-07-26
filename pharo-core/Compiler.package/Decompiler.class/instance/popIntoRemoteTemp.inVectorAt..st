instruction decoding
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	self sawClosureBytecode.
	self pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex; doStore: statements