tests - tokenizing
testFindTokensEscapedBy10

	| tokens |
	string := 'this, is, ###a, test'.
	tokens := string findTokens: ',' escapedBy: '#'.
	self assert: tokens size = 3.
	self assert: tokens third = ' #a, test'