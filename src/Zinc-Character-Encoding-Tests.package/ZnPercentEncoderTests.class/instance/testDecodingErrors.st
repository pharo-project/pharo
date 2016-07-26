testing
testDecodingErrors
	| encoder |
	encoder := ZnPercentEncoder new.
	self should: [ encoder decode: 'foo%%bar' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'fooçbar' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'foo%' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '%XX' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'foo%F' ] raise: ZnCharacterEncodingError