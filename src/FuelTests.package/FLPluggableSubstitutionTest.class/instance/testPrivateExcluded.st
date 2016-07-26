tests
testPrivateExcluded

	| result |
	self analyzer when: [:x | x isBehavior ] substituteBy: [:x | FLWeakClassMock ].

	result := self resultOfSerializeAndMaterialize: {String new. FLPair}.
	
	self assert: result first class == ByteString.
	self assert: result second == FLWeakClassMock.