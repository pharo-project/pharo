testing
testSimpleUTF8WriteStream
	| string bytes stream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := (ZnCharacterWriteStream on: ByteArray new writeStream).
	stream nextPutAll: string.
	self 
		assert: stream wrappedStream contents
		equals: bytes