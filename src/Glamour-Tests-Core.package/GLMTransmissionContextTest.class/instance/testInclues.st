tests
testInclues
	| context port |
	port := GLMSimplePort new.
	context := GLMTransmissionContext new.
	context addPort: port.
	self assert: (context includesPort: port)