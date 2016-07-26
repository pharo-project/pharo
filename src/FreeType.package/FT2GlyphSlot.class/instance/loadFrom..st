private
loadFrom: anFT2Face
	face := anFT2Face.
	self primLoadFrom: anFT2Face.
	format := ((EndianDetector isLittleEndian) ifTrue: [ format reversed ] ifFalse: [ format ]) asString.
	linearHorizontalAdvance := linearHorizontalAdvance / 65536.0.
	linearVerticalAdvance isZero ifFalse: [ linearVerticalAdvance := linearVerticalAdvance / 65536.0 ].
	advanceX := advanceX bitShift: -6.
	advanceY isZero ifFalse: [ advanceY := advanceY bitShift: -6 ].
	width := width + 63 bitShift: -6. "round up"
	height := height + 63 bitShift: -6. "round up"
	hBearingX := hBearingX bitShift: -6.
	hBearingY := hBearingY bitShift: -6.
	hAdvance := hAdvance bitShift: -6.
	vBearingX := vBearingX bitShift: -6.
	vBearingY := vBearingY bitShift: -6.
	vAdvance := vAdvance bitShift: -6.