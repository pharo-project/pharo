testing-extensions
testStringPrefixMatchesRegex
	self assert: ('aabbcc' prefixMatchesRegex: 'a+').
	self deny: ('aabbcc' prefixMatchesRegex: 'b+')