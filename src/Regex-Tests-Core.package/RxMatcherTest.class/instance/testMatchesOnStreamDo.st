testing-protocol
testMatchesOnStreamDo
	| matcher expected |
	matcher := self matcherClass forString: '\w+'.
	expected := #('now' 'is' 'the' 'time') asOrderedCollection.
	matcher 
		matchesOnStream: 'now is the time' readStream
		do: [ :each | self assert: each = expected removeFirst ].
	self assert: expected isEmpty