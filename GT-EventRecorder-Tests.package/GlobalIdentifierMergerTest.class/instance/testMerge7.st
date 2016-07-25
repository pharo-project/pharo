tests
testMerge7
	"Ensure that it converts computerUUID into string."
	stored at: #computerUUID put: UUID new.
	self assert: merger merge.
	self deny: existing = stored.
	self assert: (existing at: #computerUUID) equals: (stored at: #computerUUID) asString.
	