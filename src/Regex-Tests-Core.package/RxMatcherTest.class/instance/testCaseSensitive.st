testing-protocol
testCaseSensitive
	| matcher |
	matcher := self matcherClass forString: 'the quick brown fox' ignoreCase: false.
	self assert: (matcher search: 'the quick brown fox').
	self deny: (matcher search: 'The quick brown FOX').
	self assert: (matcher search: 'What do you know about the quick brown fox?').
	self deny: (matcher search: 'What do you know about THE QUICK BROWN FOX?')