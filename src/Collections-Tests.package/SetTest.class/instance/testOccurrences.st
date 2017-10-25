tests - sunit original tests
testOccurrences
	self assert: ((empty occurrencesOf: 0) = 0).
	self assert: ((full occurrencesOf: 4) = 1).
	full add: 4.
	self assert: ((full occurrencesOf: 4) = 1).