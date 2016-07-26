tests
testPrivateExcludedAndWithConflicts
	"Tests issue #209"
	| result |
	self analyzer when: [:x | FLPair == x ] substituteBy: [:x | FLWeakClassMock ].

	result := self resultOfSerializeAndMaterialize: {FLPair new. FLPair}.
	
	self assert: result first class == FLPair.
	self assert: result second == FLWeakClassMock