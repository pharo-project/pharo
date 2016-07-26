printing
printOn: aStream
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name;
		nextPutAll: ' default: ';
		store: default