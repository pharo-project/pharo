testing
testUTF32EncoderExampleFromD101
	| string encoder bytesBigEndianWithBom bytesLittleEndianWithBom |
	string := #(16r0000004D 16r00000430 16r00004E8C 16r00010302) collect: #asCharacter as: String.
	encoder := #utf32 asZnCharacterEncoder.
	encoder beLittleEndian.
	"start with the wrong endianness (LE)"
	bytesBigEndianWithBom := ByteArray readHexFrom: '0000FEFF0000004D0000043000004E8C00010302'.
	"the correct endianness (BE) should be detected"
	self assert: (encoder decodeBytes: bytesBigEndianWithBom) equals: string.
	self assert: encoder isBigEndian.
	"start with the wrong endianness (BE)"
	bytesLittleEndianWithBom := ByteArray readHexFrom: 'FFFE00004D000000300400008C4E000002030100'.
	"the correct endianness (LE) should be detected"
	self assert: (encoder decodeBytes: bytesLittleEndianWithBom) equals: string.
	self assert: encoder isLittleEndian.
