tests
testMerge4
	"Existing has different computer id as stored."
	existing at: #computerUUID put: 42.
	existing at: #a put: 43.
	existing at: #c put: 44.
	stored at: #computerUUID put: 21.
	stored at: #b put: 22.
	stored at: #c put: 23.
	self assert: merger merge.
	self deny: existing = stored.
	self assert: existing size equals: 4.
	self assert: stored size equals: 3.
	self assert: (existing at: #computerUUID) equals: 21 asString.
	self assert: (existing at: #a) equals: 43.
	self assert: (existing at: #b) equals: 22.
	self assert: (existing at: #c) equals: 23.
