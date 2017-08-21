testing
testDecodingErrors
	| encoder |
	encoder := ZnBase64Encoder new.
	self should: [ encoder decode: 'A' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'AB' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'ABC' ] raise: ZnCharacterEncodingError.
	encoder whitespace: #separator.
	self should: [ encoder decode: '*' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '**' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '***' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '****' ] raise: ZnCharacterEncodingError.
	encoder whitespace: nil.
	self should: [ encoder decode: '*' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '**' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '***' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '****' ] raise: ZnCharacterEncodingError.