tests
testAtGarbageCollectedValueRaisesNotFound

	self
		should: [ self dictionaryWithGarbageCollectedValue at: self keyWithGarbageCollectedValue ]
		raise: NotFound
