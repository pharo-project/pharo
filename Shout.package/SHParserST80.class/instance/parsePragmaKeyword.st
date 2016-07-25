parse
parsePragmaKeyword
	[self isKeyword]
		whileTrue: [self scanPast: #pragmaKeyword.
			self isName
				ifTrue: [self
						scanPast: (self resolvePragmaArgument: currentToken)]
				ifFalse: [self parseLiteral: false]].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd