testing-protocol
testMatchesStream
	| matcher |
	matcher := self matcherClass forString: '\w+'.
	self assert: (matcher matchesStream: 'now' readStream).
	self deny: (matcher matchesStream: 'now is' readStream)