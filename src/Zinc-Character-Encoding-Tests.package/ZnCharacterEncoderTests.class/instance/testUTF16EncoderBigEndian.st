testing
testUTF16EncoderBigEndian
	| string bytes encoder |
	string := 'élève en Français'.
	bytes := ByteArray readHexFrom:
		'00E9006C00E80076006500200065006E0020004600720061006E00E7006100690073'.
	encoder := ZnUTF16Encoder new.
	self assert: encoder isBigEndian.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string