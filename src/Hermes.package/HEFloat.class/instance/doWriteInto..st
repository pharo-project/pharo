writing
doWriteInto: aWriter
	aWriter writeUInt32: (value at:1).
	aWriter writeUInt32: (value at:2).	