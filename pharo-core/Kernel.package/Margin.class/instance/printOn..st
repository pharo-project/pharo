printing
printOn: aStream

	super printOn: aStream.

	aStream 
		nextPutAll: ' top: ';
		print: top;
		nextPutAll: ' left: ';
		print: left;
		nextPutAll: ' bottom: ';
		print: bottom;
		nextPutAll: ' right: ';
		print: right. 
	