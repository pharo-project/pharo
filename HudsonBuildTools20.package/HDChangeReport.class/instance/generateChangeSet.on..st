generating
generateChangeSet: aResolvedVersion on: aStream
	| info |
	info := aResolvedVersion version info.
	aStream tab; nextPutAll: '<logentry version="'; nextPutAll: (self encode: info name); nextPutAll: '">'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<date>'; nextPutAll: (self encode: info date yyyymmdd); space; nextPutAll: (self encode: info time print24); nextPutAll: '</date>'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<author>'; nextPutAll: (self encode: info author); nextPutAll: '</author>'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<msg>'; nextPutAll: (self encode: (self convert: info message)); nextPutAll: '</msg>'; nextPut: Character lf.
	"aStream tab; tab; nextPutAll: '<items>'; nextPut: Character lf.
	(self changesFor: aResolvedVersion)
		do: [ :each | self generateChange: each on: aStream ].
	aStream tab; tab; nextPutAll: '</items>'; nextPut: Character lf."
	aStream tab; nextPutAll: '</logentry>'; nextPut: Character lf