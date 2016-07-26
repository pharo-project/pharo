testing-protocol
testMatches
	| matcher |
	matcher := self matcherClass forString: '\w+'.
	self assert: (matcher matches: 'now').
	self deny: (matcher matches: 'now is')