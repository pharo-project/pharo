testing-protocol
testMatchesIn
	| matcher |
	matcher := self matcherClass forString: '\w+'.
	self assert: (matcher matchesIn: 'now is the time') asArray 
		= #('now' 'is' 'the' 'time')