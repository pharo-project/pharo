tests
testIsLiteral
	self assert: #[122 43 213 7] isLiteral description: 'ByteArray instances are literal'.
	self deny: thisContext method isLiteral description: 'ByteArray sub instances are not literal'