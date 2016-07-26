testing-protocol
testMatchesInDo
	| matcher expected |
	matcher := self matcherClass forString: '\w+'.
	expected := #('now' 'is' 'the' 'time') asOrderedCollection.
	matcher 
		matchesIn: 'now is the time'
		do: [ :each | self assert: each = expected removeFirst ].
	self assert: expected isEmpty