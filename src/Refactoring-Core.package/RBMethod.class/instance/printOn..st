printing
printOn: aStream 
	class printOn: aStream.
	aStream
		nextPutAll: '>>';
		nextPutAll: self selector