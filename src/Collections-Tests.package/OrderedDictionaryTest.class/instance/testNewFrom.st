tests
testNewFrom
	| dictionary |
	
	dictionary := self dictionaryClass newFrom: self orderedAssociations.
	self
		assertIsDictionary: dictionary
		withOrderedAssociations: self orderedAssociations.
	self
		assertIsDictionary: (self dictionaryClass newFrom: dictionary)
		copiedFrom: dictionary
		withOrderedAssociations: self orderedAssociations.
	self
		assertIsDictionary:
			(self dictionaryClass newFrom: self internalDictionaryWithAssociations)
		withUnorderedAssociations: self orderedAssociations.