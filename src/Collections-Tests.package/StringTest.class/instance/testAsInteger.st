tests - converting
testAsInteger

	self assert: '1796exportFixes-tkMX' asInteger = 1796.
	self assert: 'donald' asInteger isNil.
	self assert: 'abc234def567' asInteger = 234.
	self assert: '-94' asInteger = -94.
	self assert: 'foo-bar-92' asInteger = -92