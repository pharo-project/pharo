tests
testAtIfAbsentPutWithGarbageCollectedValue

	| dictionary |
	dictionary := self dictionaryWithGarbageCollectedValue.

	self
		assert:
			(dictionary
				at: self keyWithGarbageCollectedValue
				ifAbsentPut: [ 42 ])
		equals: 42.
		
	self assert: (dictionary includes: 42).
