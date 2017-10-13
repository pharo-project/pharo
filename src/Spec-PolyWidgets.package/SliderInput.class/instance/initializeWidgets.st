initialization
initializeWidgets
	slider := self instantiate: SliderPresenter.
	input := self instantiate: TextInputFieldPresenter.
	self focusOrder add: input