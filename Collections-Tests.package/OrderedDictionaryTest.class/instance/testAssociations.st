tests
testAssociations
	self
		assertSelector: #associations
		collects: self dictionaryAssociations
		from: self dictionaryWithAssociations