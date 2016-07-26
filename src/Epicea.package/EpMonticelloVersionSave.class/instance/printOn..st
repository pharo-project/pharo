printing
printOn: aStream

	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: repositoryDescription;
		nextPut: $/;
		nextPutAll: versionName;
		nextPut: $).