tests - tokenizing
testFindTokensEscapedBy12

	| tokens |
	string := 'one, two# three; four. five'.
	tokens := string findTokens: ',#;.' escapedBy: '"'.
	self assert: tokens size = 5.
	self assert: tokens third = ' three'