private
decodeLengthField

	"used in various encodings, where length field is preceeding the last trailer byte.
	Two least significant bits in last byte denoting the number of bytes for length field"
	
	| numBytes pos length |

	pos := method size.
	numBytes := ((method at: pos) bitAnd: 3) + 1.
	
	length := 0.
	1 to: numBytes do: [:i |
		length := length << 8 + (method at: pos - i ).
		].
	size := 1 + numBytes + length.
	
	^ length