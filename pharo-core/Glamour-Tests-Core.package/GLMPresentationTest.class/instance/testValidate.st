tests
testValidate
	| pane presentation | 
	pane := GLMPane named: 'test'. 
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	presentation entity: (OrderedCollection with: 1 with: 2).
	self assert: (presentation validate: 1 on: (pane port: #foo)).
	self assert: (presentation validate: 2 on: (pane port: #foo)).
	self assert: (presentation validate: 5 on: (pane port: #foo))
