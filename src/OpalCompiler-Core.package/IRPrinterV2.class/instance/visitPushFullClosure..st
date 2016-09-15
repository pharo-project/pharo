visiting
visitPushFullClosure: fullBlock

	stream nextPutAll: 'pushFullBlock: '.
	stream nextPutAll: fullBlock compiledBlock printString.
	stream nextPutAll: ' numCopied: '.
	stream nextPutAll: fullBlock copiedValues size printString.
