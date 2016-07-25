testing
testUTF32EncoderExampleFromD99
	| string bytes encoder |
	string := #(16r0000004D 16r00000430 16r00004E8C 16r00010302) collect: #asCharacter as: String.
	bytes := ByteArray readHexFrom: '0000004D0000043000004E8C00010302'.
	encoder := #utf32 asZnCharacterEncoder.
	encoder beBigEndian.
	self assert: encoder isBigEndian.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string