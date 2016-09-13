tests
testNewFrom
	| dictionary |
	
	dictionary := self dictionaryClass newFrom: self dictionaryAssociations.
	self
		assertIsDictionary: dictionary
		withAssociations: self dictionaryAssociations.
	self
		assertIsDictionary: (self dictionaryClass newFrom: dictionary)
		withAssociations: self dictionaryAssociations.
	self
		assertIsDictionary:
			(self dictionaryClass newFrom: self internalDictionaryWithAssociations)
		withUnorderedAssociations: self dictionaryAssociations.

	self
		assertDictionary: dictionary
		doesNotEqual:
			((self dictionaryClass newFrom: dictionary)
				at: self singleKey put: self newValue;
				yourself).