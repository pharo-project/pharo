tests
testPortsInmmutability
	| aPane ports |
	aPane := GLMPane in: 1.
	self assert: aPane ports isEmpty.
	ports := aPane ports.
	ports add: 1.
	self assert: ports size equals: 1.
	self assert: aPane ports isEmpty