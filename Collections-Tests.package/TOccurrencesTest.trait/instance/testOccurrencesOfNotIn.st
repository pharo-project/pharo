tests - occurrencesOf
testOccurrencesOfNotIn
	| result |
	result := self collectionWithoutEqualElements occurrencesOf: self elementNotInForOccurrences.
	self assert: result = 0