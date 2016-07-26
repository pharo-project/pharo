testing
testUTF8SurrogateCodePointsShouldFail
	| encoder surrogateCodePoint |
	encoder := #utf8 asZnCharacterEncoder.
	surrogateCodePoint := 16rD801.
	self assert: (encoder isSurrogateCodePoint: surrogateCodePoint).
	self 
		should: [ encoder encodeString: surrogateCodePoint asCharacter asString ]
		raise: ZnInvalidUTF8.
	self 
		should: [ encoder decodeBytes: #[237 160 129] ]
		raise: ZnInvalidUTF8