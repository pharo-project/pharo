tests
testAddRadioButton
	self assert: testedInstance buttons size == 0.
	testedInstance
		addRadioButton: radio1;
		addRadioButton: radio2.
	self assert: testedInstance buttons size == 2