accessing
entriesOfType: aSymbol 
	| collection |
	collection := entries select: [ :each | each type == aSymbol ].
	^ collection collect: [ :each | each contents ]