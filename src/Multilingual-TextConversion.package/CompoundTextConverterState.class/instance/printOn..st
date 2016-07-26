printing
printOn: aStream

	aStream nextPut: $(;
		nextPutAll: g0Size printString; space;
		nextPutAll: g1Size printString; space;
		nextPutAll: g0Leading printString; space;
		nextPutAll: g1Leading printString; space;
		nextPutAll: charSize printString; space;
		nextPutAll: streamPosition printString.
	aStream nextPut: $).
