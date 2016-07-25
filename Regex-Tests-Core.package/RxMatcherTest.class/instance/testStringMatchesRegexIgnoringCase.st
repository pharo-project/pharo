testing-extensions
testStringMatchesRegexIgnoringCase
	self deny: ('AABBCC' matchesRegexIgnoringCase: 'a+').
	self deny: ('AABBCC' matchesRegexIgnoringCase: 'b+c+').
	self assert: ('AABBCC' matchesRegexIgnoringCase: 'a+b+c+')