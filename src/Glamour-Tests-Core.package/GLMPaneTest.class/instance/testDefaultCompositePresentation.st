tests
testDefaultCompositePresentation

 	| pane |
	pane := GLMPane named: 'test'. 
	self assert: (pane presentations isKindOf: GLMCompositePresentation)