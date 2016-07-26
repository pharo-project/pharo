visiting
visitTempVector: tempVector

	tempVectorStack push: tempVector.
	gen pushNewArray: tempVector vars size. 
	gen storePopTemp: (self currentScope indexForVarNamed: tempVector name)