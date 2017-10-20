tests - tokenizing
testFindTokensEscapedBy15

	| tokens |
	string := 'one, "two# three"; &four. five&'.
	tokens := string findTokens: nil escapedBy: '"&'.
	self assert: tokens size = 1.
	self assert: tokens first = 'one, two# three; four. five'