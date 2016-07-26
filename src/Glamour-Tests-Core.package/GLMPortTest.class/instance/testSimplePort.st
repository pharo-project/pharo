tests
testSimplePort
	| port |
	port := GLMSimplePort new.
	port name: #foo.
	port value: 42.
	self assert: port name equals: #foo.
	self assert: port value equals: 42.
	self assert: port hasPane not