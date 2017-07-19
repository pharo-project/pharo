writing
doWriteInto: aWriter
	aWriter writeInt: value size.
	value do:[:e | e writeInto: aWriter].