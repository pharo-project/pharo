basic tests
testAdd
	"self run: #testAdd"
	"self debug: #testAdd"

	| aBag |
	aBag := Bag new.
	aBag add: 'a'.
	aBag add: 'b'.
	
	self assert: aBag size = 2.
	aBag add: 'a'.
	self assert: aBag size = 3.
	self assert: (aBag occurrencesOf: 'a') = 2
	
