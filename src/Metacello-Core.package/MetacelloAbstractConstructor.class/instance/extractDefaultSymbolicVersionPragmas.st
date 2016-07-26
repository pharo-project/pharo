pragma extraction
extractDefaultSymbolicVersionPragmas

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #defaultSymbolicVersion: into: aDict.
	^aDict