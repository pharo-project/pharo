assertions
assertIsArray: anArray withElements: aCollection
	self
		assert: anArray isArray;
		assert: anArray = aCollection asArray