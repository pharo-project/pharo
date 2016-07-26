tests
testRemoveAll
	self
		assert: specFocusOrder presenters isEmpty;
		shouldnt: [ specFocusOrder
				add: button1;
				add: button2 ]
			raise: Error;
		assert: specFocusOrder presenters size == 2;
		shouldnt: [ specFocusOrder removeAll ] raise: Error;
		assert: specFocusOrder presenters isEmpty