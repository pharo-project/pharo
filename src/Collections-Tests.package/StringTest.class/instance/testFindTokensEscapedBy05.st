tests - tokenizing
testFindTokensEscapedBy05

	| tokens |
	string := 'this, /is, a"," test/'.
	tokens := string findTokens: ',#' escapedBy: '"/'.
	self assert: tokens size = 2.
	self assert: tokens first = 'this'.
	self assert: tokens second = ' is, a"," test'.