tests
testCopy
	| context copy |
	context := GLMTransmissionContext new.
	context addPort: GLMSimplePort new.
	copy := context copy.
	self assert: copy ~~ context.
	self assert: copy ports equals: context ports.
	self assert: copy ports ~~ context ports