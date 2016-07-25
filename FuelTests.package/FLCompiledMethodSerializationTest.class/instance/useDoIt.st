helpers
useDoIt
	theClass 
		duringTestCompileSilently: 'm  ^ 42'
		storeSource: false.
	theCompiledMethod := theClass methodNamed: #m.
	theClass methodDict
		at: #DoIt
		put: theCompiledMethod