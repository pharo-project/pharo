printing
printOn: aStream
	aStream 
		nextPutAll: self class name asString;
		nextPut: $ ;
		nextPutAll: self familyName printString