tests
testMerge2
	"Existing has something. Stored has another key."
	existing at: #a put: 42.
	stored at: #b put: 21.
	self assert: merger merge.
	self deny: existing = stored.
	self assert: existing size equals: 2.
	self assert: stored size equals: 1.
	self assert: (existing at: #a) equals: 42.
	self assert: (existing at: #b) equals: 21.
	