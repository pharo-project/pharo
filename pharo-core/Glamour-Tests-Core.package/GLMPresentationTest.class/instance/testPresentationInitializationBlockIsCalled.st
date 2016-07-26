tests
testPresentationInitializationBlockIsCalled
	| pane presentation count |
	count := 0.
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new
		initialize: [ :pres |
			count := count + 1 ].
	pane addPresentation: presentation.
	self assert: count = 0.
	presentation initializePresentation.
	self assert: count = 1.
	presentation initializePresentation.
	self assert: count = 2.
