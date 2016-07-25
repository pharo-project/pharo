tests
testSimple

	| result |
	self analyzer when: [:x | x < 0 ] substituteBy: [:x | 0 ].

	result := self resultOfSerializeAndMaterialize: -1.
	
	self assert: result = 0.