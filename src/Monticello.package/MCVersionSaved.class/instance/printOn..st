printing
printOn: aStream

	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: version;
		nextPut: $@;
		nextPutAll: repository;
		nextPut: $).