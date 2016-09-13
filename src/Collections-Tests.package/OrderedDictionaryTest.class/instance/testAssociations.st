tests
testAssociations
	self
		assertSelector: #associations
		collects: #()
		from: self emptyDictionary.
	self
		assertSelector: #associations
		collects: self dictionaryAssociations
		from: self dictionaryWithAssociations.