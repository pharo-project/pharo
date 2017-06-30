tests
testSizeWithGarbageCollectedValue

	self
		assert: self dictionaryWithGarbageCollectedValue size
		equals: 0.
