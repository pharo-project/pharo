tests
testPrintStringAll
	Character allCharacters do: [ :each |
		self assert: (self class compiler
			evaluate: each printString) = each ].