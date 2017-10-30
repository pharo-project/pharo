tests
testReadWriteStreamNextNBug
	| aStream |
	aStream := ReadWriteStream on: String new.
	aStream nextPutAll: 'Hello World'.
	aStream next: 5