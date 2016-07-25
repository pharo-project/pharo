printing
printOn: aStream

	aStream 
		nextPutAll: 'MorphMargin';
		nextPutAll: ' top: ';
		nextPutAll: top printString;
		nextPutAll: ' left: ';
		nextPutAll: left printString;
		nextPutAll: ' bottom: ';
		nextPutAll: bottom printString;
		nextPutAll: ' right: ';
		nextPutAll: right printString. 
	