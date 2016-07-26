printing
printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: '(id=';
		nextPutAll: self identityHash printString;
		nextPutAll: ' title=';
		nextPutAll: self titleValue asString;
		nextPutAll: ' pane=';
		nextPutAll: self pane printString;
		nextPutAll: ')'