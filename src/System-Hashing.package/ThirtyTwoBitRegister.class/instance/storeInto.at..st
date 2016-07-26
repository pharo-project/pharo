accessing
storeInto: aByteArray at: index
	"Store my 32-bit value into the four bytes of the given ByteArray starting at the given index. Consider the first byte to contain the most significant bits of the word (i.e., use big-endian byte ordering)."

	aByteArray at: index put: (hi bitShift: -8).
	aByteArray at: index + 1 put: (hi bitAnd: 16rFF).
	aByteArray at: index + 2 put: (low bitShift: -8).
	aByteArray at: index + 3 put: (low bitAnd: 16rFF)