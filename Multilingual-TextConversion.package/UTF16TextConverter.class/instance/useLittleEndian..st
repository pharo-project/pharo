accessing
useLittleEndian: aBoolean
	aBoolean = self useLittleEndian ifFalse: [self swapLatin1EncodingByteOrder].
	useLittleEndian := aBoolean.
