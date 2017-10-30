tests
testAtIfPresentIfAbsentWithGarbageCollectedValue

	self
		assert:
			(self dictionaryWithGarbageCollectedValue
				at: self keyWithGarbageCollectedValue
				ifPresent: [ 41 ]
				ifAbsent: [ 42 ])
		equals: 42
