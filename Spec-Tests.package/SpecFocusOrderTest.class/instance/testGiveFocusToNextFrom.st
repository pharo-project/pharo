tests
testGiveFocusToNextFrom
	self
		assert: specFocusOrder presenters isEmpty;
		shouldnt: [ specFocusOrder
				add: button1;
				add: button2 ]
			raise: Error;
			assert: (specFocusOrder giveFocusToNextFrom:button1 for: nil).