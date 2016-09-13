tests
testDo
	self
		assertSelector: #do:
		enumerates: #()
		from: self emptyDictionary.
	self
		assertSelector: #do:
		enumerates: self dictionaryValues
		from: self dictionaryWithAssociations.