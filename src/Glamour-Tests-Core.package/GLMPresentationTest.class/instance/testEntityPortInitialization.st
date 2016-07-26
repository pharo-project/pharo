tests
testEntityPortInitialization
	| pane presentation |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new
		initialize: [ :pres |
			pres entity: 2  ].
	pane addPresentation: presentation.
	self assert: presentation entity = nil.
	presentation initializePresentation.
	self assert: (pane port: #entity) value = 2.
	self assert: presentation entity = 2.