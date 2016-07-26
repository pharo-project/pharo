initialization
initialize

	super initialize.

	debugActionHolder := nil asValueHolder.
	debugActionHolder whenChangedDo: [ self update ]
	
	
	