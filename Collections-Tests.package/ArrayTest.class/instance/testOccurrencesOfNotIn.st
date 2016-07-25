tests - occurrencesOf
testOccurrencesOfNotIn
	| result |
	result := self empty occurrencesOf: self elementNotInForOccurrences.
	self assert: result = 0