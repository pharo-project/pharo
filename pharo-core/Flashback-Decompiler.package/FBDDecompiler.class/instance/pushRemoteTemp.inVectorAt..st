data flow instructions
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	simulatedStack addLast: ((simulatedStack at: tempVectorIndex + 1) at: remoteTempIndex + 1)