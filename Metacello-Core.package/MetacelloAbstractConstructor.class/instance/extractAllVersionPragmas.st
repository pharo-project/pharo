pragma extraction
extractAllVersionPragmas

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #version: into: aDict.
	self extractPragmas: #version:imports: into: aDict.
	^aDict