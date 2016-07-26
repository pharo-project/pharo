tests - converting
testAsInteger
	self assert: '1796exportFixes-tkMX' asWideString asInteger = 1796.
	self assert: 'donald' asWideString asInteger isNil.
	self assert: 'abc234def567' asWideString asInteger = 234.
	self assert: '-94' asWideString asInteger = -94.
	self assert: 'foo-bar-92' asWideString asInteger = -92.

	self assert: '1796exportFixes-tkMX' asWideString asSignedInteger = 1796.
	self assert: 'donald' asWideString asSignedInteger isNil.
	self assert: 'abc234def567' asWideString asSignedInteger = 234.
	self assert: '-94' asWideString asSignedInteger = -94.
	self assert: 'foo-bar-92' asWideString asSignedInteger = -92.

	self assert: '1796exportFixes-tkMX' asWideString asUnsignedInteger = 1796.
	self assert: 'donald' asWideString asUnsignedInteger isNil.
	self assert: 'abc234def567' asWideString asUnsignedInteger = 234.
	self assert: '-94' asWideString asUnsignedInteger = 94.
	self assert: 'foo-bar-92' asWideString asUnsignedInteger = 92