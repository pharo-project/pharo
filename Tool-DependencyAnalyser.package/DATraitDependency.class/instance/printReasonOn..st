printing
printReasonOn: aStream
	aStream
		nextPutAll: self theClass name;
		nextPutAll: ' uses ';
		print: self trait name