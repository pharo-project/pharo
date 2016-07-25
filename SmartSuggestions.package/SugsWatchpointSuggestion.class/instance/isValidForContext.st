execution
isValidForContext
	context ifNil: [ ^false ].
	^context hasUnacceptedEdits not
	