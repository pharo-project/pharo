basic tests
testOccurrencesOf
	"self debug: #testOccurrencesOf"

	| aBag |
 	aBag := Bag new.	
	aBag add: 'a' withOccurrences: 3.
	aBag add: 'b'.
	aBag add: 'b'.
	aBag add: 'b'.
	aBag add: 'b'.	
	self assert: (aBag occurrencesOf:'a') = 3.
	self assert: (aBag occurrencesOf:'b') = 4.
	self assert: (aBag occurrencesOf:'c') = 0.
	self assert: (aBag occurrencesOf: nil) =0.
	aBag add: nil.
	self assert: (aBag occurrencesOf: nil) =1.
	