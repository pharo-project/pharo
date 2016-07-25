testing
testUTF16EncoderLittleEndian
	| string bytes encoder |
	string := 'élève en Français'.
	bytes := ByteArray readHexFrom:
		'E9006C00E80076006500200065006E0020004600720061006E00E700610069007300'.
	encoder := ZnUTF16Encoder new.
	encoder beLittleEndian.
	self assert: encoder isLittleEndian.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string