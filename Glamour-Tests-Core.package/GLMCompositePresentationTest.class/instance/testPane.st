tests
testPane
	| pane composite presentation |
	pane := GLMPane new.
	composite := GLMCompositePresentation new.
	presentation := GLMPresentation new.
	composite add: presentation.
	composite pane: pane.
	self assert: composite pane == pane.
	self assert: composite first pane == pane