parse
parseMethodTemporaries
	currentTokenFirst == $|
		ifFalse: [ ^ self ].
	self scanPast: #methodTempBar.
	[ self isName ] whileTrue: [ self scanPast: #patternTempVar ].
	self failUnless: currentToken = '|'.
	self scanPast: #methodTempBar