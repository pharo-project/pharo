tests
testKeysSortedSafely
	self
		assertSelector: #keysSortedSafely
		collects: #()
		from: self emptyDictionary.
	"The keys are all strings, so the default #sorted implementation is OK."
	self
		assertSelector: #keysSortedSafely
		collects: self dictionaryKeys sorted
		from: self dictionaryWithAssociations.