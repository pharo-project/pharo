printing
printOn: aStream
	aStream nextPutAll: 'a TextFontReference(';
		print: font;
		nextPut: $)