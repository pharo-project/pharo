tests
testAddSetsCorrectPane
	| composite presentation |
	composite := GLMCompositePresentation new.
	presentation := GLMPresentation new.
	composite add: presentation.
	self assert: composite pane equals: presentation pane