testing-extensions
testStringPrefixMatchesRegexIgnoringCase
	self assert: ('AABBCC' prefixMatchesRegexIgnoringCase: 'a+').
	self deny: ('AABBCC' prefixMatchesRegexIgnoringCase: 'b+')