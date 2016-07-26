tests - occurrencesOf
testOccurrencesOfEmpty
	| result |
	result := self empty occurrencesOf: self elementInForOccurrences.
	self assert: result = 0