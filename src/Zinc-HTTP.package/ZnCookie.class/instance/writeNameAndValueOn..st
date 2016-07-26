printing
writeNameAndValueOn: aStream
	aStream
		nextPutAll: self name;
		nextPutAll: '=';
		nextPutAll: self value