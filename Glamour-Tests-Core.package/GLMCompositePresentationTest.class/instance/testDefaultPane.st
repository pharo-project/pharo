tests
testDefaultPane
	| composite |
	composite := GLMCompositePresentation new.
	self assert: composite pane presentations equals: composite