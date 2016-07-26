testing
hasPragmaNamed: aSymbol	
	self pragmaNamed: aSymbol ifAbsent: [ ^ false ].
	^ true