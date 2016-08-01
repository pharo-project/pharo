tests
testToggleState
	self deny: testedInstance state.
	testedInstance toggleState.
	self assert: testedInstance state