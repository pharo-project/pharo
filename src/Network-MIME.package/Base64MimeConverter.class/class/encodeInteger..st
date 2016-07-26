encoding
encodeInteger: int
	| strm |
	"Encode an integer of any length and return the MIME string"

	strm := ReadWriteStream on: (ByteArray new: int digitLength).
	1 to: int digitLength do: [:ii | strm nextPut: (int digitAt: ii)].
	strm reset.
	^ ((self mimeEncode: strm) contents) copyUpTo: $=	"remove padding"