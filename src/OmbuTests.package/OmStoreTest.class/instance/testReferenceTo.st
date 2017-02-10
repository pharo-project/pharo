tests
testReferenceTo
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.
	
	self beforeAndAfterFlushStore: [
		aReference := store referenceTo: anEntry.

		self assert: (aReference isKindOf: OmReference).
		self deny: aReference isNull ]