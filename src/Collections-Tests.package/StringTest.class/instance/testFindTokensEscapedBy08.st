tests - tokenizing
testFindTokensEscapedBy08

	| tokens |
	string := 'this, is, ##a, test'.
	tokens := string findTokens: ',' escapedBy: '#'.
	self assert: tokens size = 4.
	self assert: tokens third = ' a'.
	self assert: tokens fourth = ' test'