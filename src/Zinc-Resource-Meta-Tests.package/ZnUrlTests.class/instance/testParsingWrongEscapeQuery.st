testing
testParsingWrongEscapeQuery
	self should: [ ZnUrl fromString: 'http://foo:8080/foo?%%bar=1' ] raise: ZnCharacterEncodingError 