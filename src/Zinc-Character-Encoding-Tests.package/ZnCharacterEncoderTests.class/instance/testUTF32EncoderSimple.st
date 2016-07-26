testing
testUTF32EncoderSimple
	| string bytes encoder |
	string := 'élève en Français'.
	bytes := ByteArray readHexFrom: '000000e90000006c000000e8000000760000006500000020000000650000006e000000200000004600000072000000610000006e000000e7000000610000006900000073'.
	encoder := #utf32 asZnCharacterEncoder.
	self assert: encoder isBigEndian.
	self assert: (encoder encodedByteCountForString: string) equals: 17 * 4.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string