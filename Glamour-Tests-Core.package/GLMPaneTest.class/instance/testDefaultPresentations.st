tests
testDefaultPresentations
	| pane |
	pane := GLMPane named: 'test'.
	self assert: (pane presentations isKindOf: GLMCompositePresentation)