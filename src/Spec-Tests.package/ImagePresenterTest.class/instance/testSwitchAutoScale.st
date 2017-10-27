tests
testSwitchAutoScale
	| autoState |
	autoState := testedInstance autoScale.
	testedInstance switchAutoscale.
	self assert: testedInstance autoScale equals: autoState not