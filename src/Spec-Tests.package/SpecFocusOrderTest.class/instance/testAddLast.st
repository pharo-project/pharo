tests
testAddLast
	self
		assert: specFocusOrder presenters isEmpty;
		shouldnt: [ specFocusOrder
				addLast: button1;
				addLast: button2]
			raise: Error;
		assert: specFocusOrder presenters
			equals:
			{button1.
			button2} asOrderedCollection