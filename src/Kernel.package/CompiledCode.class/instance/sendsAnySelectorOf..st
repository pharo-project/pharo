testing
sendsAnySelectorOf: aCollection
	
	(self localSendsAnySelectorOf: aCollection) ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb sendsAnySelectorOf: aCollection ]
