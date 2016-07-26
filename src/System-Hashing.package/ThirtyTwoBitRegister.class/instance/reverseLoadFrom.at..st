accessing
reverseLoadFrom: aByteArray at: index
	"Load my 32-bit value from the four bytes of the given ByteArray
starting at the given index. Consider the first byte to contain the most
significant bits of the word (i.e., use big-endian byte ordering)."

	hi := ((aByteArray at: index + 3) bitShift: 8) + ( aByteArray at: index + 2).
	low := ((aByteArray at: index + 1) bitShift: 8) + ( aByteArray at: index).
