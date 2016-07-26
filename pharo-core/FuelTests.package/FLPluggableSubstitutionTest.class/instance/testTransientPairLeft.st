tests
testTransientPairLeft

	| result |
	self analyzer when: [:x | FLPair = x class and: [x left isNil not]] substituteBy: [:x | x copy left: nil].

	result := self resultOfSerializeAndMaterialize: (FLPair new left: $A; yourself).
	
	self assert: result left isNil.