tests
testIllegalCharacterEscapes
	self should: [ STON fromString: '''\a''' ] raise: STONReaderError.
	self should: [ STON fromString: '''\u''' ] raise: STONReaderError.
	self should: [ STON fromString: '''\u00''' ] raise: STONReaderError.
	self should: [ STON fromString: '''\u000''' ] raise: STONReaderError.
	self should: [ STON fromString: '''\*''' ] raise: STONReaderError