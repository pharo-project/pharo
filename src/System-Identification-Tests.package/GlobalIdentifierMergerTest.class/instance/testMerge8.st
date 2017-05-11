tests
testMerge8
	"Ensure that it converts secretUUID into string."
	stored at: #secretUUID put: UUID new.
	self assert: merger merge.
	self deny: existing = stored.
	self assert: (existing at: #secretUUID) equals: (stored at: #secretUUID) asString.
	