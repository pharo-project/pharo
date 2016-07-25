testing-protocol
testMatchingRangesIn
	| matcher expected |
	matcher := self matcherClass forString: '\w+'.
	expected := #(1 3 5 6 8 10 12 15) asOrderedCollection.
	(matcher matchingRangesIn: 'now is the time') do: [ :range |
		self assert: range first = expected removeFirst.
		self assert: range last = expected removeFirst ].
	self assert: expected isEmpty