parse
parsePragmaBinary
	self scanPast: #pragmaBinary.
	self isName
		ifTrue: [self
				scanPast: (self resolvePragmaArgument: currentToken)]
		ifFalse: [self parseLiteral: false].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd