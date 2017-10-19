tests
testIsOutdated

	self deny: store isOutdated.
	store newEntry: (OmEntry content: 42).
	self deny: store isOutdated.
	store flush.
	self deny: store isOutdated.
