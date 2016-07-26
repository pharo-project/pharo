decoding
nextEncodedUint16
	"Answer the next unsigned, 16-bit integer from this (binary) stream."
	^ (stream next bitShift: 8) + (stream next).
	