tests
testLookaround
	self assert: ('A'   matchesRegex: '(?!ABC).*').
	self assert: ('AB'  matchesRegex: '(?!ABC).*').
	self deny:   ('ABC' matchesRegex: '(?!ABC).*').