testing-extensions
testStringAllRegexMatches
	| result |
	result := 'aabbcc' allRegexMatches: 'b+'.
	self assert: result size = 1.
	self assert: result first = 'bb'