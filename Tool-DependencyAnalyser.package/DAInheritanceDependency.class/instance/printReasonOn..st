printing
printReasonOn: aStream
	aStream
		nextPutAll: self theClass name;
		nextPutAll: ' inherits from ';
		nextPutAll: self superclass name