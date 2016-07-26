tests
testAddingFocusOrder
	radioGroupModel
		addRadioButton: radio1;
		addRadioButton: radio2.
	self shouldnt: [ radioGroupModel takeKeyboardFocus ] raise: Error.
	self assert: (radioGroupModel giveFocusToNextFrom: radio1)