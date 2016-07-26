visiting
visitTempVector: tempVector

	stream nextPutAll: 'createTempVectorNamed: '.
	tempVector name printOn: stream.
	stream nextPutAll: ' withVars: '.
	stream nextPutAll: tempVector vars printString.
	
	