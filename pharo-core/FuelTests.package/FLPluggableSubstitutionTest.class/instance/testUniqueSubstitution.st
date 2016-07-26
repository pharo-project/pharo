tests
testUniqueSubstitution
	"Tests that an object with two occurrences in the graph is substituted to the *same* object."

	| result |
	self analyzer when: [ :x | x isNumber ] substituteBy: [ :x | x printString ].

	"This line demonstrates that behavior being tested is not trivial"
	self deny: 1 printString == 1 printString.

	result := self resultOfSerializeAndMaterialize: (Array with: 1 with: 1).
	
	self assert: result first = '1'.	
	self assert: result first == result second.