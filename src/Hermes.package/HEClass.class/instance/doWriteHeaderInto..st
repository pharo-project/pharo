writing
doWriteHeaderInto: aWriter
	aWriter writeByteString: className.
	aWriter writeByteString: superclass.
	aWriter writeByteString: instanceVariables.
	aWriter writeByteString: classInstancevariables.
	aWriter writeByteString: classVariables.
	aWriter writeByteString: sharedPools.
	aWriter writeByteString: layoutClass