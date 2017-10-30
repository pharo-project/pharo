convenience
encodedByteCountForCodePoints: codePoints
	"Return the exact number of bytes it would take to encode codePoints as a byte array"
	"Overwritten for performance reasons"

	^ codePoints size