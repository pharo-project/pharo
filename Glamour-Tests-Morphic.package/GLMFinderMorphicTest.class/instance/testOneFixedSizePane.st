tests
testOneFixedSizePane
	| finder |
	finder := GLMFinder new fixedSizePanes: 1.
	window := finder openOn: 40.
	self assert: window submorphs last submorphs first submorphs first width equals: window submorphs last width.
	(finder panes first port: #selection) value: 42.
	self assert: window submorphs last submorphs last submorphs size equals: 3.
	self
		assert: window submorphs last submorphs last submorphs first extent
		equals: window submorphs last submorphs last submorphs last extent.
	self assert: window submorphs last submorphs last submorphs first width equals: window submorphs last width