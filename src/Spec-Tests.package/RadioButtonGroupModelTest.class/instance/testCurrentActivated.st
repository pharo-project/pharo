tests
testCurrentActivated
	radioGroupModel addRadioButton: radio1.
	self assert: radioGroupModel currentActivated equals: nil.
	radio1 click.
	self assert: radioGroupModel currentActivated equals: radio1