tests
testHeadReference
	1 to: 7 do: [ :each | store newEntry: (OmEntry content: each) ].

	self beforeAndAfterFlushStore: [
		| lastEntry |
		lastEntry := store entries detect: [ :entry | entry content = 7 ].
		self
			assert: store headReference
			equals: (store referenceTo: lastEntry) ]