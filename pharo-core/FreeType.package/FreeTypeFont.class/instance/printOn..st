printing
printOn: aStream

	aStream
		nextPutAll: self class name;
		nextPut: $(;
		print: face familyName;
		space;
		print: face styleName;
		space;
		print: pointSize;
		nextPut: $)