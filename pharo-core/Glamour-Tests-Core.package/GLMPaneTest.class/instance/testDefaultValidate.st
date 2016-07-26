tests
testDefaultValidate
	| pane |
	pane := GLMPane named: 'test'. 
	self assert: ((pane port: #selection) validate: 5)
