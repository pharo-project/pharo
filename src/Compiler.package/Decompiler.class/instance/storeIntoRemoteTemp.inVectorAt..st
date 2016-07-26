instruction decoding
storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	self sawClosureBytecode.
	self pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex; doStore: stack