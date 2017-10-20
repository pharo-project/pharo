tests - tokenizing
testFindTokensEscapedBy16

	| tokens |
	string := 'one, "two# three"; &four. five&'.
	tokens := string findTokens: nil escapedBy: nil.
	self assert: tokens size = 1.
	self assert: tokens first = string