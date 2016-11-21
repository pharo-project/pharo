printing
printOn: aStream

	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: version info name;
		nextPut: $@;
		nextPutAll: repository description;
		nextPut: $).