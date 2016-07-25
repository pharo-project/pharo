generating
generateChangeRemoval: aPatch on: aStream
	aStream tab; tab; tab; nextPutAll: '<item action="remove">'; nextPutAll: (self encode: (self convert: aPatch summary)); nextPutAll: '</item>'; nextPut: Character lf