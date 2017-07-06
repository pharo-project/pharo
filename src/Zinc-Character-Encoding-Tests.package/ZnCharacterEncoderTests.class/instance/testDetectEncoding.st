testing
testDetectEncoding
	| bytes |
	bytes := 'abc' asByteArray.
	self assert: (ZnCharacterEncoder detectEncoding: bytes) equals: ZnCharacterEncoder ascii.
	bytes := ZnCharacterEncoder iso88591 encodeString: 'Les élèves Français'.
	self assert: (ZnCharacterEncoder detectEncoding: bytes) equals: ZnCharacterEncoder iso88591.
	bytes := ZnCharacterEncoder utf8 encodeString: 'Les élèves Français'.
	self assert: (ZnCharacterEncoder detectEncoding: bytes) equals: ZnCharacterEncoder utf8.
