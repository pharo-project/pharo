tests
testKeysDo
	self
		assertSelector: #keysDo:
		enumerates: self dictionaryKeys
		from: self dictionaryWithAssociations