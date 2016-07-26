tests
testEqualsDictionary
	self
		assertDictionary: self emptyDictionary
		doesNotEqual: self emptyInternalDictionary.
	self
		assertDictionary: self dictionaryWithAssociations
		doesNotEqual: self internalDictionaryWithAssociations.