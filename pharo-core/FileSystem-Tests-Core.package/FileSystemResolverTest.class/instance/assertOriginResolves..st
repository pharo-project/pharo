asserting
assertOriginResolves: aSymbol
	| reference |
	reference := resolver resolve: aSymbol.
	self assert: (reference isKindOf: FileReference).
	self assert: reference isAbsolute.
	self assert: reference exists.
	^ reference