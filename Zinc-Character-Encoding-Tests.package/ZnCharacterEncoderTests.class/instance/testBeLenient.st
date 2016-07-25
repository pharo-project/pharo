testing
testBeLenient
	| encoder unmapped |
	encoder := ZnCharacterEncoder newForEncoding: 'iso-8859-1'.
	unmapped := (128 to: 159) asByteArray.
	self should: [ encoder encodeString: unmapped asString ] raise: ZnCharacterEncodingError.
	self should: [ encoder decodeBytes: unmapped ] raise: ZnCharacterEncodingError.
	encoder beLenient.
	self assert: (encoder encodeString: unmapped asString) equals: unmapped.
	self assert: (encoder decodeBytes: unmapped) equals: unmapped asString.