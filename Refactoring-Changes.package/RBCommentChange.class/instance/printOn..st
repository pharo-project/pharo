printing
printOn: aStream 
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' classComment: ';
		print: (self comment copyReplaceAll: '!' with: '!!');
		nextPutAll: ' stamp: ';
		print: (self changeStamp);
		nextPutAll: '!'