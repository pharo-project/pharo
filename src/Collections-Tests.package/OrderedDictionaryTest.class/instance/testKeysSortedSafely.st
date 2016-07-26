tests
testKeysSortedSafely
	"The keys are all strings, so the default #sorted implementation will suffice."
	self
		assertSelector: #keysSortedSafely
		collects: self dictionaryKeys sorted
		from: self dictionaryWithAssociations