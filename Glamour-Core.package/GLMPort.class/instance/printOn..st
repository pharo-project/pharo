printing
printOn: aStream 
	super printOn: aStream.
	aStream 
		nextPut: Character space;
		nextPutAll: self identityHash printString;
		nextPutAll: ' (name='; 
		nextPutAll: self name printString; 
		nextPutAll: ' value='; 
		nextPutAll: self value printString; 
		nextPut: $)