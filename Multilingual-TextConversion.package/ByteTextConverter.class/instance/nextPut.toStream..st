conversion
nextPut: unicodeCharacter toStream: aStream
	"Write the unicode character to aStream."

	aStream isBinary
		ifTrue: [aStream basicNextPut: unicodeCharacter charCode]
		ifFalse: [aStream basicNextPut: (self unicodeToByte: unicodeCharacter)]