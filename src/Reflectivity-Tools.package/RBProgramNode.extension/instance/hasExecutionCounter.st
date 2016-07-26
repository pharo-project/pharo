*Reflectivity-Tools-Watchpoints
hasExecutionCounter
	
	self hasMetalinkBefore ifFalse: [ ^false ].
	^self beforeLinks anySatisfy: [ :link | link metaObject class = ExecutionCounter ]