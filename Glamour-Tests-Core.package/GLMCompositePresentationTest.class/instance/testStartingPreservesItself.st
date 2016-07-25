tests
testStartingPreservesItself
	| composite finder |
	composite := GLMCompositePresentation new.
	finder := composite finder.
	composite startOn: 42.
	self assert: composite pane presentations equals: composite.
	self assert: composite first == finder