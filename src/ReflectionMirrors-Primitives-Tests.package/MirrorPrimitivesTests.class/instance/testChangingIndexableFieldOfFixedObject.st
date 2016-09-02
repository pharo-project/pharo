tests-fields accessing
testChangingIndexableFieldOfFixedObject
	
	self should: [MirrorPrimitives indexableFieldOf: 10@20 at: 1 put: 10] raise: Error