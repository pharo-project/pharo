tests
testAskBeforeChanging
	| state |
	self assert: testedInstance askBeforeChanging not.
	self openInstance.
	state := testedInstance widget askBeforeChanging.
	self assert: state not.
	testedInstance askBeforeChanging: true.
	state := testedInstance widget askBeforeChanging.
	self assert: state