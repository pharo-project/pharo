printing
printOn: aStream 
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeFromSystem';
		nextPut: $!