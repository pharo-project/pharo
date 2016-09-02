tests-fields accessing
testGettingIndexableFieldOfFixedObject
	
	self should: [MirrorPrimitives indexableFieldOf: 10@20 at: 1] raise: Error 