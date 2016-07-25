*Reflectivity-Tools
hasBreakpoint
	
	self hasMetalinkBefore ifFalse: [ ^false ].
	^self beforeLinks anySatisfy: [ :link | link metaObject = Break ]