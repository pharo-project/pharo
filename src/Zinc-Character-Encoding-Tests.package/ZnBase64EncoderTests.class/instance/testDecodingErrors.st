testing
testDecodingErrors
	| encoder |
	encoder := ZnBase64Encoder new.
	self should: [ encoder decode: 'A' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'AB' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'ABC' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '*' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '**' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '***' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '****' ] raise: ZnCharacterEncodingError.