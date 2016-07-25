accessing
strcpy: aString
	handle := ByteArray new: aString size + 1.
	^ self primStrcpy: aString.