basic tests
testAddWithOccurrences
	"self debug:#testAddWithOccurrences"
	
	| aBag |
 	aBag := Bag new.	
	aBag add: 'a' withOccurrences: 3.
	self assert: (aBag size = 3).
	
	
	
	
		
