testing
testByteEncoderFromUrl
	| encoderFromUrl iso88592 bytes characters |
	encoderFromUrl := ZnByteEncoder newFromUrl: 'http://files.pharo.org/extra/unicode/8859-2.TXT'.
	iso88592 := ZnCharacterEncoder newForEncoding: #iso88592.
	self assert: encoderFromUrl byteDomain equals: iso88592 byteDomain.
	self assert: encoderFromUrl characterDomain equals: iso88592 characterDomain.
	bytes := encoderFromUrl byteDomain.
	self 
		assert: (encoderFromUrl decodeBytes: bytes)
		equals: (iso88592 decodeBytes: bytes).
	characters := String withAll: encoderFromUrl characterDomain asArray.
	self 
		assert: (encoderFromUrl encodeString: characters)
		equals: (iso88592 encodeString: characters).
		