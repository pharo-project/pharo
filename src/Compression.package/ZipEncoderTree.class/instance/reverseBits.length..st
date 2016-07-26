private
reverseBits: code length: length
	"Bit reverse the given code"
	| result bit bits |
	result := 0.
	bits := code.
	1 to: length do:[:i|
		bit := bits bitAnd: 1.
		result := result << 1 bitOr: bit.
		bits := bits >> 1].
	^result