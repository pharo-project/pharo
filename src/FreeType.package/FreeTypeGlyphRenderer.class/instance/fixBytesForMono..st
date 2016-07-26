private
fixBytesForMono: aGlyphForm
	"On Windows, the bits in each byte are in reverse order, and inverted.
	i.e. 2r10100000 should be 2r11111010  to display correctly.
	This needs further investigation"
	| b newB bits |
	bits := aGlyphForm bits.
	1 to: bits byteSize do:[:i |
		b := bits byteAt: i.
		newB := ((((((((b bitAnd: 2r10000000) bitShift: -7)
			bitOr: ((b bitAnd: 2r1000000) bitShift: -5))
			bitOr: ((b bitAnd: 2r100000) bitShift: -3))
			bitOr: ((b bitAnd: 2r10000) bitShift: -1))
			bitOr: ((b bitAnd: 2r1000) bitShift: 1))
			bitOr: ((b bitAnd: 2r100) bitShift: 3))
			bitOr: ((b bitAnd: 2r10) bitShift: 5))
			bitOr: ((b bitAnd: 2r1) bitShift: 7).
		bits byteAt: i put: (newB bitXor: 2r11111111)].
	^aGlyphForm