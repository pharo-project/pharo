breakpoints
hasUnacceptedEdits
	context ifNil: [ ^false ].
	^context hasUnacceptedEdits
	