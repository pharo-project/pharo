tests
testRebuildWidget
	radioGroupModel
		addRadioButton: radio1;
		addRadioButton: radio2.
	self shouldnt: [ radioGroupModel radio2 ] raise: Error.
	self assert: radioGroupModel buttons size == 2