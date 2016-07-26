tests
testDo
	self
		assertSelector: #do:
		enumerates: self dictionaryValues
		from: self dictionaryWithAssociations