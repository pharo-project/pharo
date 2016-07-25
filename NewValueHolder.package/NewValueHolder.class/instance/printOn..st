printing
printOn: aStream
	super printOn: aStream.
	
	aStream 
		nextPutAll: '[ '; 
		print: self value;
		nextPutAll: ' ]'