helpers
useNotInstalled
	theClass 
		duringTestCompileSilently: 'm  ^ 42'
		storeSource: false.
	theCompiledMethod := theClass methodNamed: #m.
	theClass methodDict removeKey: #m