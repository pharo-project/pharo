writing
writeInto: aWriter
	aWriter writeByteString: name.
	aWriter writeByteString: className.
	aWriter writeByteString: protocol.
	aWriter writeInt: header.
	aWriter writeByteArray: bytecode.
	
	aWriter writeInt: literals size.
	literals do:[:e | e writeInto: aWriter].