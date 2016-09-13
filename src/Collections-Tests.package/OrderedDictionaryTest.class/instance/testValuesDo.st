tests
testValuesDo
	self
		assertSelector: #valuesDo:
		enumerates: #()
		from: self emptyDictionary.
	self
		assertSelector: #valuesDo:
		enumerates: self dictionaryValues
		from: self dictionaryWithAssociations.