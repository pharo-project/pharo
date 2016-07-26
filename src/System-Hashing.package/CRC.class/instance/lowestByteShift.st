private-bit-manipulation
lowestByteShift
	^ lowestByteShift ifNil: [ lowestByteShift := width - 8 ]