initialization
initializeWidgets
	input := self instantiate: TextInputFieldPresenter.
	dropList := self instantiate: DropListPresenter.
	self focusOrder
		add: input;
		add: dropList