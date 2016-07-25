tests
testMethodWithStyle
	| reader |
	reader := MCStReader on: self methodWithStyle readStream.
	self assert: reader definitions first isMethodDefinition.