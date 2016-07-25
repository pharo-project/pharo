testing-protocol
testMatchesOnStream
	| matcher |
	matcher := self matcherClass forString: '\w+'.
	self assert: (matcher matchesOnStream: 'now is the time' readStream) asArray 
		= #('now' 'is' 'the' 'time')