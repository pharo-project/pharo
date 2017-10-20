generating
generateStatsOn: aStream
	aStream tab; nextPutAll: '<stats>'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<packages value="'; print: (packages size); nextPutAll: '"/>'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<classes value="'; print: (packages detectSum: [ :each | each classes size ]); nextPutAll: '"/>'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<methods value="'; print: (packages detectSum: [ :each | each methods size ]); nextPutAll: '"/>'; nextPut: Character lf.
	aStream tab; nextPutAll: '</stats>'; nextPut: Character lf.