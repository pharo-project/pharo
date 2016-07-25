parse
parseBlockTemporaries
	currentTokenFirst == $|
		ifFalse: [ ^ self ].
	self scanPast: #blockTempBar.
	[ self isName ] whileTrue: [ self scanPast: #blockPatternTempVar ].
	self failUnless: currentToken = '|'.
	self scanPast: #blockTempBar