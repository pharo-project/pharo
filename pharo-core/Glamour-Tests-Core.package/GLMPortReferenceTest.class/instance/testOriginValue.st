tests
testOriginValue
	| port reference |
	port := GLMSimplePort new value: 42.
	reference := GLMOriginPortReference new port: port.
	self assert: reference value equals: 42