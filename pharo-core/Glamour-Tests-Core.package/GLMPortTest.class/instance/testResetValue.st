tests
testResetValue
	| port |
	port := GLMTestPane new port: #port1.
	port value: 42.
	self assert: port value equals: 42.
	port resetValue.
	self assert: port value isNil