writing
doWriteInto: aWriter
	aWriter writeByte:(value ifTrue:1 ifFalse:0).