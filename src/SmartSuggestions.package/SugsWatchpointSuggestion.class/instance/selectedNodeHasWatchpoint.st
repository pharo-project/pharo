private - testing
selectedNodeHasWatchpoint
	context ifNil: [ ^false ].
	^context selectedNode hasWatchpoint