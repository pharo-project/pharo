tests
testDefaultPortValue
	| pane |
	pane := GLMPane named: 'test'.
	self assert: (pane port: #somePort) value isNil