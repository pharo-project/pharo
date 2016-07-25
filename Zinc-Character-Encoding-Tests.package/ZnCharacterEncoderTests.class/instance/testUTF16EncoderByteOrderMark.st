testing
testUTF16EncoderByteOrderMark
	| string bytes encoder encoded |
	string := 'foo'.
	bytes := ByteArray readHexFrom: 'FEFF0066006f006f'.
	encoder := ZnUTF16Encoder new.
	self assert: encoder isBigEndian.
	encoded := ByteArray streamContents: [ :out |
		encoder nextPutByteOrderMarkToStream: out.
		encoder next: string size putAll: string startingAt: 1 toStream: out ].
	self assert: encoded equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string.
	encoder beLittleEndian.
	self assert: encoder isLittleEndian.
	self assert: (encoder decodeBytes: bytes) equals: string.
	self assert: encoder isBigEndian.
	1 to: bytes size by: 2 do: [ :each | bytes swap: each with: each + 1 ].
	self assert: (encoder decodeBytes: bytes) equals: string.
	self assert: encoder isLittleEndian
