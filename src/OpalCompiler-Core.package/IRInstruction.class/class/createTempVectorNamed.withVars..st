instance creation
createTempVectorNamed: aTempVectorName withVars: anArray  

	^ IRTempVector new
		name: aTempVectorName;
		vars: anArray;
		yourself.