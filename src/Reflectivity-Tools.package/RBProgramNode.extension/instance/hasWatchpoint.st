*Reflectivity-Tools-Watchpoints
hasWatchpoint
	
	self hasMetalinkAfter ifFalse: [ ^false ].
	^self afterLinks anySatisfy: [ :link | link metaObject class = Watchpoint ]