tests
testAvoidRecursion

	| result |
	self analyzer when: [:x | x < 10 ] substituteBy: [:x | x + 1 ].

	result := self resultOfSerializeAndMaterialize: 1.
	
	self assert: result = 2.