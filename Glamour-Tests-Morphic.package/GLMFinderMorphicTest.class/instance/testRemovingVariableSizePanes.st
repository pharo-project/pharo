tests
testRemovingVariableSizePanes
	| finder |
	finder := GLMFinder new variableSizePanes.
	window := finder openOn: 40.
	self assert: window submorphs last scrollTarget submorphs size equals: 2.
	(finder panes first port: #selection) value: 41.
	self assert: window submorphs last scrollTarget submorphs size equals: 4.
	(finder panes first port: #selection) value: 42.
	self assert: window submorphs last scrollTarget submorphs size equals: 4.
	(finder panes second port: #selection) value: 43.
	self assert: window submorphs last scrollTarget submorphs size equals: 6.
	(finder panes first port: #selection) value: 44.
	self assert: window submorphs last scrollTarget submorphs size equals: 4