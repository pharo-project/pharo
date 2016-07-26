converting
encodedByteCountForCodePoint: codePoint
	"Return how many bytes are needed to encode integer codePoint"

	codePoint > self maximumUTFCode 
		ifTrue: [ ^ self errorOutsideRange ].
	^ 4