tests
testMerge3
	"Existing has same key as stored."
	existing at: #a put: 42.
	stored at: #a put: 21.
	self deny: merger merge.
	self deny: existing = stored.
	self assert: existing size equals: 1.
	self assert: stored size equals: 1.
	self assert: (existing at: #a) equals: 42.
	