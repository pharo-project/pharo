tests
testSubstituteByItself

	| result |
	self analyzer when: [:x | true ] substituteBy: [:x | x ].

	result := self resultOfSerializeAndMaterialize: 1.
	
	self assert: result = 1.