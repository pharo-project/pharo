pragma extraction
extractVersionImportPragmas

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #version:imports: into: aDict.
	^aDict