tests - tokenizing
testFindTokensEscapedBy13

	| tokens |
	string := 'one, two# three; four. five'.
	tokens := string findTokens: ',#;.' escapedBy: nil.
	self assert: tokens size = 5.
	self assert: tokens third = ' three'