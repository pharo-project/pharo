tests
testAddingFocusOrder
	testedInstance
		addRadioButton: radio1;
		addRadioButton: radio2.
	self shouldnt: [ testedInstance takeKeyboardFocus ] raise: Error.
	self assert: (testedInstance giveFocusToNextFrom: radio1)