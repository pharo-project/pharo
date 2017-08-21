initialization
initializeWidgets
	dropList := self instantiate: DropListModel.
	button := self instantiate: ButtonModel.
	self focusOrder
		add: dropList;
		add: button