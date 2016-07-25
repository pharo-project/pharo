tests
testAdd
	self
		assert: specFocusOrder presenters isEmpty;
		shouldnt: [ specFocusOrder
				add: button1;
				add: button2;
				add: button1;
				add: button1 ]
			raise: Error;
		assert: specFocusOrder presenters
			equals:
			{button1.
			button2.
			button1.
			button1} asOrderedCollection