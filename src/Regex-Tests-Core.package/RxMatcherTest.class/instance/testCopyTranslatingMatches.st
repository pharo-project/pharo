testing-protocol
testCopyTranslatingMatches
	| matcher |
	matcher := self matcherClass forString: '\w+'.
	self assert: (matcher copy: 'now is  the   time    ' translatingMatchesUsing: [ :each | each reversed ])
		= 'won si  eht   emit    '