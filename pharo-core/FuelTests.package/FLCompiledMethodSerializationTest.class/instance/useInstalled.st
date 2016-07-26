helpers
useInstalled
	theClass 
		duringTestCompileSilently: 'm  ^ 42'
		storeSource: true.
	theCompiledMethod := theClass methodNamed: #m