writing
doWriteInto: aWriter
	aWriter writeByteString: className.
	aWriter writeByteString: selector.

