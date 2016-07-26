pragma extraction
extractSymbolicVersionPragmas

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #symbolicVersion: into: aDict.
	^aDict