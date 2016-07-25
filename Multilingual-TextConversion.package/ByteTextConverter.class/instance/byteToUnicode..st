conversion
byteToUnicode: char
	"Map from my byte based encoding to unicode.
	Due to the leading char encoding this is not strictly true, but hopefully at some point we can get rid of the leading char overhead."
	| value |
	value := char charCode.

	value < 128
		ifTrue: [^ char].
	value > 255
		ifTrue: [^ char].
	^self class byteToUnicodeTable at: (value - 128 + 1)