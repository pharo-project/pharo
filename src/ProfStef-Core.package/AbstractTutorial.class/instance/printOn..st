printing
printOn: aStream
	aStream 
		nextPutAll: 'a Pharo Tutorial (';
		nextPutAll: self class title;
		nextPutAll: ')'.    