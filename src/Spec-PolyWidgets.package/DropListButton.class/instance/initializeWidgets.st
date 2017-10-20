initialization
initializeWidgets
	dropList := self instantiate: DropListPresenter.
	button := self instantiate: ButtonPresenter.
	self focusOrder
		add: dropList;
		add: button