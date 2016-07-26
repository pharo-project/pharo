tests
testAssociationsDo
	self
		assertSelector: #associationsDo:
		enumerates: self dictionaryAssociations
		from: self dictionaryWithAssociations