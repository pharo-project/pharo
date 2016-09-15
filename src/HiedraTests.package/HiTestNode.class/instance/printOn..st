printing
printOn: aStream 

	super printOn: aStream.	
	
	aStream 
		nextPut: $(;
		print: content;
		nextPutAll: '->';
		print: parents asArray;
		nextPut: $).