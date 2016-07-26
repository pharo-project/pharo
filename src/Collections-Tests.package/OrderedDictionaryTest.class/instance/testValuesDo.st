tests
testValuesDo
	self
		assertSelector: #valuesDo:
		enumerates: self dictionaryValues
		from: self dictionaryWithAssociations