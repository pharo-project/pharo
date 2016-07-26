tests
testKeysAndValuesDo
	self
		assertSelector: #keysAndValuesDo:
		enumerates: self dictionaryKeys
		and: self dictionaryValues
		from: self dictionaryWithAssociations