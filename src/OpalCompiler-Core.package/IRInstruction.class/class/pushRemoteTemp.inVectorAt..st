instance creation
pushRemoteTemp: aName inVectorAt: nameOfVector
	^ IRPushRemoteTemp new
		name: aName;
		tempVectorName: nameOfVector;
		yourself.