execution
selectedNodeHasExecutionCounter
	context ifNil: [ ^false ].
	^context selectedNode hasExecutionCounter