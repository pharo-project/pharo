parse
parsePrimitive
	self scanNext.
	currentTokenFirst isDigit
		ifTrue: [self scanPast: #integer]
		ifFalse: [self parseStringOrSymbol].
	currentToken = 'module:'
		ifTrue: [self scanPast: #module.
			self parseStringOrSymbol.
			currentToken = 'error:'
				ifTrue: [self scanPast: #primitive.
					self isName
						ifTrue: [self scanPast: #patternTempVar]
						ifFalse: [self parseStringOrSymbol]]]
		ifFalse: [currentToken = 'error:'
				ifTrue: [self scanPast: #module.
					self isName
						ifTrue: [self scanPast: #patternTempVar]
						ifFalse: [self parseStringOrSymbol].
					currentToken = 'module:'
						ifTrue: [self scanPast: #primitive.
							self parseStringOrSymbol]]].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd