tests
testAddRadioButton
	self assert: radioGroupModel buttons size == 0.
	radioGroupModel
		addRadioButton: radio1;
		addRadioButton: radio2.
	self assert: radioGroupModel buttons size == 2