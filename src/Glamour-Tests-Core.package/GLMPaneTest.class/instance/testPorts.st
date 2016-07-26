tests
testPorts
	| pane port |
	pane := GLMPane named: 'test'.
	self assert: pane ports isEmpty.
	port := pane port: #foo.
	self assert: pane ports size equals: 1.
	self assert: pane ports first == port.
	self assert: (pane port: #foo) == port