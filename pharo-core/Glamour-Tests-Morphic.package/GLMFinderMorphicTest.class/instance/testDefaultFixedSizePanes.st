tests
testDefaultFixedSizePanes
	| finder container |
	finder := GLMFinder new.
	window := finder openOn: 40.
	window width: 522.
	container := self find: TransformMorph in: window.
	self assert: container submorphs first width equals: window submorphs last width.
	(finder panes first port: #selection) value: 42.
	self assert: container submorphs size equals: 3.
	self
		assert: container submorphs first extent
		equals: container submorphs last extent.
	self assert: container submorphs first width <= (window submorphs last width / 2)