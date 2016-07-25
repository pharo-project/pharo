basic tests
testAsSet

	| aBag aSet |
	aBag := Bag new.	
	aBag add:'a' withOccurrences: 4.
	aBag add:'b' withOccurrences: 2.
	aSet := aBag asSet.
	self assert: aSet size = 2.
	self assert: (aSet occurrencesOf: 'a') = 1 
	