tests
testMerge
	"Existing is empty. Stored has value."
	stored at: #a put: 42.
	self assert: merger merge.
	self assert: existing equals: stored.
	self assert: (existing at: #a) equals: 42.
	