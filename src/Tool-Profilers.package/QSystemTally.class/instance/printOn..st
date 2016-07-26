printing
printOn: aStream
	aStream print: class; nextPutAll: '>>'; print: (method ifNotNil:[method selector]).
	aStream nextPutAll: ' -- '; print: tally.