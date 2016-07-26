tests
testPages
	self assert: PharoTutorialHelp pages isCollection.
	self assert: (PharoTutorialHelp pages allSatisfy: #isSymbol).