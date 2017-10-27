tests
testAtIfPresentWithGarbageCollectedValue

	| dictionary |
	dictionary := self dictionaryWithGarbageCollectedValue.

	self
		assert:
			(dictionary
				at: self keyWithGarbageCollectedValue
				ifPresent: [ 42 ])
		equals: nil.
