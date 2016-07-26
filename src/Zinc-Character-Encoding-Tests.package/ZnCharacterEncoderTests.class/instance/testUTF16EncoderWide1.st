testing
testUTF16EncoderWide1
	| string bytes encoder |
	string := (Character codePoint: 16r1d11e) asString. "MUSICAL SYMBOL G CLEF"
	bytes := ByteArray readHexFrom: 'D834DD1E'.
	encoder := ZnUTF16Encoder new.
	self assert: encoder isBigEndian.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string