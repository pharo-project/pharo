tests - occurrencesOf
testOccurrencesOfEmpty
	| result |
	result := self empty occurrencesOf: (self collectionWithoutEqualElements anyOne).
	self assert: result = 0