testing
testLatin1Encoder
	| encoder string bytes |
	encoder := ZnCharacterEncoder newForEncoding: 'latin1'.
	string := 'élève en Français'.
	bytes := #(233 108 232 118 101 32 101 110 32 70 114 97 110 231 97 105 115) asByteArray.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string