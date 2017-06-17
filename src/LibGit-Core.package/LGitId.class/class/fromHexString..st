instance creation
fromHexString: aString
	^ self fromByteArray: (ByteArray readHexFrom: aString)