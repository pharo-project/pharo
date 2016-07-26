tests
testFixedSize
	window := GLMFinder new openOn: 123.
	self assert: window submorphs last class equals: GLMPaneScroller