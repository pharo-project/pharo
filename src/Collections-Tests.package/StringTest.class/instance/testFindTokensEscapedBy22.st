tests - tokenizing
testFindTokensEscapedBy22

	| tokens |
	string := ''.
	tokens := string findTokens: $, escapedBy: $".
	self assert: tokens size = 0