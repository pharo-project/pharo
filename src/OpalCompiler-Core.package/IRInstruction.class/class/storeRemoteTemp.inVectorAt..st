instance creation
storeRemoteTemp: aName inVectorAt: nameOfVector
	^ IRStoreRemoteTemp new
		name: aName;
		tempVectorName: nameOfVector;
		yourself.