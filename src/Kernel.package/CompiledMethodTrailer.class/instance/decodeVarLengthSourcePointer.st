decoding
decodeVarLengthSourcePointer

	| pos shift |
	
	pos := method size-1.
	shift := data := 0.
	
	[ | value | 
		value := method at: pos.
		data := (value bitAnd: 16r7F) << shift + data.
		pos := pos - 1.
		shift := shift + 7.
		value > 127 ] whileTrue.

	size := method size - pos.