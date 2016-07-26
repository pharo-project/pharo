parse
parseKeywordMessagePattern
	[self isKeyword]
		whileTrue: [self scanPast: #patternKeyword.
			self failUnless: self isName.
			self scanPast: #patternArg]