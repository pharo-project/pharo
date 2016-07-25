testing
testParsingWrongPort
	self should: [ ZnUrl fromString: 'http://foo:bar' ] raise: ZnPortNotANumber   