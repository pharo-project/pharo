testing
testLocalDirectory
	| reference |
	reference := resolver resolve: #localDirectory.
	self assert: (reference isKindOf: FileReference).
	^ reference