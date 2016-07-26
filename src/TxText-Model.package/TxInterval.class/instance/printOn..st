printing
printOn: aStream

	super printOn: aStream.
	aStream 
		nextPut: $[;
		print: start.
		
	
		aStream 
			nextPutAll: ' .. ';
			print: end.
	
	
	aStream
		nextPut: $]