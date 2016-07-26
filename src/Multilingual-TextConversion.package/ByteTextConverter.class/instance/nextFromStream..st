conversion
nextFromStream: aStream
	"Read the next byte (we are only dealing with byte based encodings here) character from aStream and return the result converted to unicode."

	| byteOrChar |
	byteOrChar := aStream basicNext.
	aStream isBinary
		ifTrue: [^byteOrChar].
	^byteOrChar
		ifNotNil: [self byteToUnicode: byteOrChar]