testing
testParsingWrongEscape
	self should: [ ZnUrl fromString: 'http://foo:8080/foo%%bar' ] raise: ZnCharacterEncodingError 