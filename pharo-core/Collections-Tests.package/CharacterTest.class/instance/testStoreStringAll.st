tests
testStoreStringAll
	Character allCharacters do: [ :each |
		self assert: (self class compiler
			evaluate: each storeString) = each ].