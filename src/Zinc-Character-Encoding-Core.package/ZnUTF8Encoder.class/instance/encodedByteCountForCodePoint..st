converting
encodedByteCountForCodePoint: codePoint
	"Return how many bytes are needed to encode integer code point"

	codePoint < 128 ifTrue: [ ^ 1 ].
	codePoint < 2048 ifTrue: [ ^ 2 ].
	codePoint < 65535 ifTrue: [ ^ 3 ].
	codePoint <= self maximumUTFCode ifTrue: [ ^ 4 ].
	self errorOutsideRange.
	^ 0