parse
parseBlockArguments
	[currentTokenFirst == $:]
		whileTrue: [self scanPast: #blockArgColon.
			self failUnless: self isName.
			self scanPast: #blockPatternArg].
	currentTokenFirst == $|
		ifTrue: [^ self scanPast: #blockArgsBar]