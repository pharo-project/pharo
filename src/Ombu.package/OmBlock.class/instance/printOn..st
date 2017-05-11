printing
printOn: aStream

	super printOn: aStream.
	
	aStream 
		nextPutAll: '(first entry: ';
		print: self firstLocalNameAsInteger;
		nextPutAll: ' positions: ';
		print: startPosition;
		nextPutAll: '..';
		print: endPosition;
		nextPut: $).
