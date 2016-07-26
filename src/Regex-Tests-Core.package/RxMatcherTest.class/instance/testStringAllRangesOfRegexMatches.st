testing-extensions
testStringAllRangesOfRegexMatches
	| result |
	result := 'aabbcc' allRangesOfRegexMatches: 'b+'.
	self assert: result size = 1.
	self assert: result first first = 3.
	self assert: result first last = 4
	