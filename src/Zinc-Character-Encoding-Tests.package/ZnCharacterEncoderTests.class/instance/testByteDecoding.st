testing
testByteDecoding
	| encoder bytes |
	encoder := ZnUTF8Encoder new.
	bytes := encoder encodeString: 'élève en Français'.
	self assert: (bytes decodeWith: encoder) equals: (encoder decodeBytes: bytes).
	self assert: (bytes decodeWith: #utf8) equals: (encoder decodeBytes: bytes).
	self assert: bytes utf8Decoded equals: (encoder decodeBytes: bytes)