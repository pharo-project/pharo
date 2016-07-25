pragma extraction
extractCommonDefaultSymbolicVersionPragmas

	| aDict |
	aDict := Dictionary new.
	self extractPragmas: #defaultSymbolicVersion: for: ConfigurationOf into: aDict.
	^aDict