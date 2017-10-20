tests - tokenizing
testFindTokensEscapedBy07

	| tokens |
	string := 'a:b::c'.
	tokens := string findTokens: ':' escapedBy: '"'.
	self assert: tokens size = 4.
	self assert: tokens first = 'a'.
	self assert: tokens second = 'b'.
	self assert: tokens third = ''.
	self assert: tokens fourth = 'c'