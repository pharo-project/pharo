tests
testBindingsDo
	self
		assertSelector: #bindingsDo:
		enumerates: #()
		from: self emptyDictionary.
	self
		assertSelector: #bindingsDo:
		enumerates: self dictionaryAssociations
		from: self dictionaryWithAssociations.