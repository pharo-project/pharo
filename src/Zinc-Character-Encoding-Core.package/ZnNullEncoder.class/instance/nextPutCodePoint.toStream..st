converting
nextPutCodePoint: codePoint toStream: stream
	"Write the encoding for Integer code point to stream"

	codePoint < 256
		ifTrue: [ stream nextPut: codePoint ]
		ifFalse: [ self errorOutsideRange ]