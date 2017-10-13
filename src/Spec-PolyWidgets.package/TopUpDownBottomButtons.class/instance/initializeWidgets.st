initialization
initializeWidgets
	topBottom := self instantiate: TopBottomButtons.
	upDown := self instantiate: UpDownButtons.

	self focusOrder
		add: topBottom;
		add: upDown