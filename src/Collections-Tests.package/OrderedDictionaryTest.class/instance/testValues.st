tests
testValues
	self
		assertSelector: #values
		collects: #()
		from: self emptyDictionary.
	self
		assertSelector: #values
		collects: self dictionaryValues
		from: self dictionaryWithAssociations.