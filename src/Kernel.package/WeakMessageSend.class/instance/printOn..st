printing
printOn: aStream

	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		print: selector;
		nextPutAll: ' -> ';
		print: self receiver;
		nextPut: $).
