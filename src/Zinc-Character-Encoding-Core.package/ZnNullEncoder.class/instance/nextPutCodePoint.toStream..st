converting
nextPutCodePoint: codePoint toStream: stream
	codePoint < 256
		ifTrue: [ stream nextPut: codePoint ]
		ifFalse: [ self errorOutsideRange ]