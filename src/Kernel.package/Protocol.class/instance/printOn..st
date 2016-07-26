printing
printOn: aStream

	aStream 
		nextPutAll: (self class name);
		nextPutAll: ' (';
		nextPutAll: (self name);
		nextPutAll: ') - ';
		nextPutAll: (self methods size asString);
		nextPutAll: ' selector(s)'.