encoding
encodeSourcePointer

	encodedData := ByteArray new: 4.
	encodedData at: 4 put: (data >> 24) + 251.
	1 to: 3 do: [:i |
		encodedData at: 4-i put: ((data bitShift: (i-3)*8) bitAnd: 16rFF)]