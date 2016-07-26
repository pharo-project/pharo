parse
parseMessagePattern
	self isName
		ifTrue: [self parseUnaryMessagePattern]
		ifFalse: [self isBinary
				ifTrue: [self parseBinaryMessagePattern]
				ifFalse: [self failUnless: self isKeyword.
					self parseKeywordMessagePattern]]