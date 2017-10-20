tests - tokenizing
testFindTokensEscapedBy03

	| tokens |
	string := 'this, is, a, test'.
	tokens := string findTokens: ',' escapedBy: '"'.
	self assert: tokens size = 4