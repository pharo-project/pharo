testing-protocol
testMatchesInCollect
	| matcher |
	matcher := self matcherClass forString: '\w+'.
	self assert: (matcher
		matchesIn: 'now is the time'
		collect: [ :each | each reversed ]) asArray
			= #('won' 'si' 'eht' 'emit')