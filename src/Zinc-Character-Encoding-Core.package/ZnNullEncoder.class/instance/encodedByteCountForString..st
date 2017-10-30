convenience
encodedByteCountForString: string
	"Return the exact number of bytes it would take to encode string as a byte array"
	"Overwritten for performance reasons"

	^ string size