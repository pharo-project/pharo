tests - tokenizing
testFindTokensEscapedBy14

	| tokens |
	string := 'one, "two# three"; &four. five&'.
	tokens := string findTokens: ',#;.' escapedBy: '"&'.
	self assert: tokens size = 3.
	self assert: tokens second = ' two# three'.
	self assert: tokens third = ' four. five'