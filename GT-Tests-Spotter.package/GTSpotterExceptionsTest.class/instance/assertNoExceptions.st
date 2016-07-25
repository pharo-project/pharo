private
assertNoExceptions
	self assert: self class exceptions isEmpty.
	self assert: self class fatals isEmpty.