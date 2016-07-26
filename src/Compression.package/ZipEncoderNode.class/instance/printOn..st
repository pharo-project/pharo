printing
printOn: aStream
	super printOn: aStream.
	aStream nextPut:$(;
		nextPutAll:'value = '; print: value;
		nextPutAll:', freq = '; print: frequency;
		nextPutAll:', bitLength = '; print: bitLength;
		nextPutAll:', code = '; print: code;
		nextPutAll:', height = '; print: height; 
	nextPut:$).