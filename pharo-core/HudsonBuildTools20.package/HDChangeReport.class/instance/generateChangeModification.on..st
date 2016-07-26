generating
generateChangeModification: aPatch on: aStream
	aStream tab; tab; tab; nextPutAll: '<item action="change">'; nextPutAll: (self encode: (self convert: aPatch summary)); nextPutAll: '</item>'; nextPut: Character lf