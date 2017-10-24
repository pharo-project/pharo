tests
testLinkRadioButton
	testedInstance
		addRadioButton: radio1;
		addRadioButton: radio2.
	self assert: radio1 state == false.
	self assert: radio2 state == false.
	radio1 click.
	self assert: radio1 state == true.
	self assert: radio2 state == false.
	radio2 click.
	self assert: radio1 state == false.
	self assert: radio2 state == true