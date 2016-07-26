tests - occurrencesOf
testOccurrencesOf
	| result expected |
	result := self nonEmpty occurrencesOf: self elementInForOccurrences.
	expected := 0.
	self nonEmpty do: [ :each | self elementInForOccurrences = each ifTrue: [ expected := expected + 1 ] ].
	self assert: result = expected