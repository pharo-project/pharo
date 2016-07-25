instructions
storePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex 
	stack pop.
	encoder genStorePopRemoteTemp: tempIndex - 1 inVectorAt: tempVectorIndex - 1