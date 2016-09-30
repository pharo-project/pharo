scanning
hasInstVarRef
	self localHasInstVarRef ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb hasInstVarRef ]