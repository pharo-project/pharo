private
encodeLengthField: integer
	| bytes value |
	[integer > 0] assert.
	value := integer.
	bytes := ByteArray streamContents: [:str |
		[ value > 0 ] whileTrue: [
			str nextPut: (value bitAnd: 255).
			value := value >> 8 ]].
	"no more than 4 bytes for length field"
	[bytes size <=4] assert.
	^ bytes