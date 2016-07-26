instruction decoding
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	self sawClosureBytecode.
	stack addLast: ((tempVars at: tempVectorIndex + 1) remoteTemps at: remoteTempIndex + 1)