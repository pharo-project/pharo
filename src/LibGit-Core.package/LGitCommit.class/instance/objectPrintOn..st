printing
objectPrintOn: aStream
	self id hexString in: [ :hexId |
		aStream
			nextPutAll: (hexId first: 7);
			nextPutAll: '...';
			nextPutAll: (hexId last: 7);
			nextPutAll: ', ';
			nextPutAll: self message surroundedBySingleQuotes ]