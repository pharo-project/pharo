writing
doWriteInto: aWriter
	aWriter writeInt32: value size.
	value do:[:e | e writeInto: aWriter].