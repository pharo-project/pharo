pragma extraction
extractVersionPragmas

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #version: into: aDict.
	^aDict