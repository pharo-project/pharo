writing
writeInto: aWriter
	aWriter writeByte: self class tag.
	self doWriteInto: aWriter.