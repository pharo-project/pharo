tests
testDefault
	testedInstance addRadioButton: radio1.
	self assert: radio1 state == false