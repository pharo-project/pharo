printing
printOn: aStream
	aStream nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self package packageName;
		nextPut: $)