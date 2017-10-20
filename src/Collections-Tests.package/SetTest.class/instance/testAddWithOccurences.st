tests - some
testAddWithOccurences
 
	empty add: 2 withOccurrences: 3.
	self assert: (empty includes: 2).
	self assert: ((empty occurrencesOf: 2) = 1).