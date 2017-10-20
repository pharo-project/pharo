tests-categorization
testNonCharacterNegative
	self aRandomSelectionOfCharactersDo: [  :ch |
		self deny: (Unicode isNonCharacter: ch) 
	]