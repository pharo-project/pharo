scanning
sendsToSuper
	self localSendsToSuper ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb sendsToSuper ]