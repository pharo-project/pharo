tests
testPresentationInitializationBlock
	| pane presentation count|
	count := 0.
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new
		initialize: [ :pres |
			(pres pane port: #test)  value: count ].
	pane addPresentation: presentation.
	self assert: (pane port: #test) value = nil.
	presentation initializePresentation.
	self assert: (pane port: #test) value = 0.
	self assert: (presentation pane port: #test) value = 0.
	count := 1.
	presentation initializePresentation.
	self assert: (pane port: #test) value = 1.
	self assert: (presentation pane port: #test) value = 1.