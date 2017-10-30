tests
testAtIfAbsentWithGarbageCollectedValue

	self
		assert:
			(self dictionaryWithGarbageCollectedValue
				at: self keyWithGarbageCollectedValue
				ifAbsent: [ 42 ])
		equals: 42
