initialization
initializeWidgets
	firstButton := self instantiate: ButtonPresenter.
	secondButton := self instantiate: ButtonPresenter.
	self initializeButtons.
	self focusOrder
		add: firstButton;
		add: secondButton