parse
parsePragmaSequence
	[currentToken = '<']
		whileTrue: [self scanPast: #primitiveOrExternalCallStart.
			currentToken = 'primitive:'
				ifTrue: [self rangeType: #primitive.
					self parsePrimitive]
				ifFalse: [self isTokenExternalFunctionCallingConvention
						ifTrue: [self rangeType: #externalFunctionCallingConvention.
							self parseExternalCall]
						ifFalse: [self isName
								ifTrue: [self scanPast: #pragmaUnary.
									self failUnless: currentToken = '>'.
									self scanPast: #primitiveOrExternalCallEnd]
								ifFalse: [self isKeyword
										ifTrue: [self parsePragmaKeyword]
										ifFalse: [self isBinary
												ifTrue: [self parsePragmaBinary]
												ifFalse: [self error]]]]]]