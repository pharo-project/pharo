tests
testKeys
	self
		assertSelector: #keys
		collects: #()
		from: self emptyDictionary.
	self
		assertSelector: #keys
		collects: self dictionaryKeys
		from: self dictionaryWithAssociations.