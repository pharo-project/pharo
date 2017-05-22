printing
objectPrintOn: aStream
	aStream
		nextPutAll: self name;
		nextPutAll: ' <';
		nextPutAll: self email;
		nextPutAll: '> '.
	self dateAndTime printOn: aStream