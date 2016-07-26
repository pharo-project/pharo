printing
printOn: aStream

	super printOn: aStream.
	debuggingName ifNotNil: [aStream nextPutAll: debuggingName ].  