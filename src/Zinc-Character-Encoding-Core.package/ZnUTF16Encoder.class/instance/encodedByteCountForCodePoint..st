converting
encodedByteCountForCodePoint: codePoint
	"Return how many bytes are needed to encode integer code point"

	codePoint <= 65535 ifTrue: [ ^ 2 ].
	codePoint <= self maximumUTFCode ifTrue: [ ^ 4 ].
	self errorOutsideRange