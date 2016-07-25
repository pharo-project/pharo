generating
generateChangeAddition: aPatch on: aStream
	aStream tab; tab; tab; nextPutAll: '<item action="add">'; nextPutAll: (self encode: (self convert: aPatch summary)); nextPutAll: '</item>'; nextPut: Character lf