testing-extensions
testStringCopyWithRegexMatchesReplacedWith
	self assert: ('aabbcc' copyWithRegex: 'b+' matchesReplacedWith: 'X') = 'aaXcc'