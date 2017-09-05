initialization
initializeWidgets
	firstButton := self instantiate: ButtonModel.
	secondButton := self instantiate: ButtonModel.
	self initializeButtons.
	self focusOrder
		add: firstButton;
		add: secondButton