visiting
visitDirectory: anEntry
	| reference |
	reference := anEntry reference.
	reference = source
		ifTrue: [dest ensureCreateDirectory]
		ifFalse: [self copyDirectory: reference]