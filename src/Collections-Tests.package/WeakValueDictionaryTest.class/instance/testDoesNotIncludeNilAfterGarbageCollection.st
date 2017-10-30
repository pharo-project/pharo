tests
testDoesNotIncludeNilAfterGarbageCollection

	self deny: (self dictionaryWithGarbageCollectedValue includes: nil)