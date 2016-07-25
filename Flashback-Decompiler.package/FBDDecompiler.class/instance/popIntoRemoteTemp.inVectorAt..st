data flow instructions
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	self storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex.
	self doPop.