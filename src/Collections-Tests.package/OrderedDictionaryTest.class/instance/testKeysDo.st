tests
testKeysDo
	self
		assertSelector: #keysDo:
		enumerates: #()
		from: self emptyDictionary.
	self
		assertSelector: #keysDo:
		enumerates: self dictionaryKeys
		from: self dictionaryWithAssociations.