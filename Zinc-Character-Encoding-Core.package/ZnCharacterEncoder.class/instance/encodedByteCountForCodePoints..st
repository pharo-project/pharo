convenience
encodedByteCountForCodePoints: codePoints
	"Return the exact number of bytes it would take to encode codePoints as a byte array"

	^ codePoints 
		inject: 0 
		into: [ :sum :each |
			sum + (self encodedByteCountForCodePoint: each) ]