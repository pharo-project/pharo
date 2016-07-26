printing
storeOn: aStream

	aStream 
		nextPutAll: 'LogicalFont';
		nextPutAll: '
		familyName: '; nextPutAll: self familyName printString ;
		nextPutAll: '
		pointSize: '; nextPutAll: self pointSize asString;
		nextPutAll: '
		stretchValue: '; nextPutAll: self stretchValue asString;
		nextPutAll: '
		weightValue: '; nextPutAll: self weightValue asString;
		nextPutAll: '
		slantValue: '; nextPutAll: self slantValue asString.