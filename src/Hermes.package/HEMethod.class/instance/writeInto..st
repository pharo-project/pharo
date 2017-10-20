writing
writeInto: aWriter
	aWriter writeByteString: name.
	aWriter writeByteString: className.
	aWriter writeByteString: protocol.

	aWriter writeByteString: encoderClassName.
	aWriter writeUInt32: numArgs.
	aWriter writeUInt32: numTemps.
	aWriter writeUInt32: hasPrimitive.

	aWriter writeByteArray: bytecode.
	
	aWriter writeInt32: literals size.
	literals do:[:e | e writeInto: aWriter].