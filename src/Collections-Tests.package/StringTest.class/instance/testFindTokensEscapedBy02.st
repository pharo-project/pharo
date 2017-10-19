tests - tokenizing
testFindTokensEscapedBy02

	| tokens |
	string := ''.
	tokens := string findTokens: ',' escapedBy: '"'.
	self assert: tokens isEmpty