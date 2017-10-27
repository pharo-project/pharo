tests
testCurrentActivated
	testedInstance addRadioButton: radio1.
	self assert: testedInstance currentActivated equals: nil.
	radio1 click.
	self assert: testedInstance currentActivated equals: radio1