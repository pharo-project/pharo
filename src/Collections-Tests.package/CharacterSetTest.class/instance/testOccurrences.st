sunit original tests
testOccurrences
	self assert: (empty occurrencesOf: $w) equals: 0.
	self assert: (full occurrencesOf: $w) equals: 1.
	full add: $w.
	self assert: (full occurrencesOf: $w) equals: 1