tests
testEqualsDictionary
	self
		assertDictionary: self emptyDictionary
		doesNotEqual: self emptyInternalDictionary.

	self
		assertDictionary: self dictionaryWithOrderedAssociations
		doesNotEqual: self internalDictionaryWithAssociations.