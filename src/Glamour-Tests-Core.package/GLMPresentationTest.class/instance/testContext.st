tests
testContext
	| pane presentation |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	presentation selection: 42.
	self assert: (pane port: #selection) value equals: 42.
	(pane port: #entity) value: 'foo'.
	self assert: presentation entity equals: 'foo'