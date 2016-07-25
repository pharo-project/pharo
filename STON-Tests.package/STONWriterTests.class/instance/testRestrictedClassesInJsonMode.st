tests
testRestrictedClassesInJsonMode
	self should: [ self serializeJson: 1@2 ] raise: STONWriterError.
	self should: [ self serializeJson: STONTestUser dummy ] raise: STONWriterError.
