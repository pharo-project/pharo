testing-extensions
testStringMatchesRegex
	self deny: ('aabbcc' matchesRegex: 'a+').
	self deny: ('aabbcc' matchesRegex: 'b+c+').
	self assert: ('aabbcc' matchesRegex: 'a+b+c+')