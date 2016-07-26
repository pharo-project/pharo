visiting
visitFile: anEntry
	| reference |
	reference := anEntry reference.
	reference = source
		ifTrue: [source copyTo: dest]
		ifFalse: [self copyFile: reference]