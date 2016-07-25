printing
printOn: aStream
	aStream
		nextPutAll: 'aSugsSuggestion (';
		nextPutAll: self label;
		nextPutAll: ')'.