tests
testAssociationsDo
	self
		assertSelector: #associationsDo:
		enumerates: #()
		from: self emptyDictionary.
	self
		assertSelector: #associationsDo:
		enumerates: self dictionaryAssociations
		from: self dictionaryWithAssociations.