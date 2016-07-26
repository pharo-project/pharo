tests
testNewFrom
	| dictionaryOne dictionaryTwo dictionaryThree |
	
	dictionaryOne := self dictionaryClass newFrom: self dictionaryAssociations.
	dictionaryTwo := self dictionaryClass newFrom: dictionaryOne.
	dictionaryThree := self dictionaryClass newFrom: self internalDictionaryWithAssociations.
	self
		assertIsDictionary: dictionaryOne withAssociations: self dictionaryAssociations;
		assertIsDictionary: dictionaryTwo withAssociations: self dictionaryAssociations;
		assertIsDictionary: dictionaryThree includesAll: self dictionaryAssociations.

	(self dictionaryClass newFrom: dictionaryOne)
		at: self singleKey put: self newValue.
	self deny: (dictionaryOne at: self singleKey) = self newValue.