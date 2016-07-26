tests
testValidate
	| pane presentation |
	pane := GLMPane named: 'test'. 
	presentation := GLMListPresentation new.
	pane addPresentation: presentation.
	presentation entity: (OrderedCollection with: 1 with: 2).
	presentation shouldValidate: true.
	self assert: ((pane port: #selection) validate: 1).
	self assert: ((pane port: #selection) validate: 2).
	self deny: ((pane port: #selection) validate: 5)
