printing
printOn: aStream
	super printOn: aStream.
	aStream
		cr;
		nextPutAll: ' familyName: ';
		nextPutAll: familyName asString;
		cr;
		nextPutAll: ' emphasis: ';
		nextPutAll: emphasis asString;
		cr;
		nextPutAll: ' pointSize: ';
		nextPutAll: pointSize asString;
		cr;
		nextPutAll: ' realFont: ';
		nextPutAll: realFont asString;
		nextPutAll: ' weight: ';
		nextPutAll: weightValue asString;
		nextPutAll: ' stretch: ';
		nextPutAll: stretchValue asString;
		nextPutAll: ' slant: ';
		nextPutAll: slantValue asString