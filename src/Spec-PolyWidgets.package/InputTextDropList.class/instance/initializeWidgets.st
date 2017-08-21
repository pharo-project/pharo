initialization
initializeWidgets
	input := self instantiate: TextInputFieldModel.
	dropList := self instantiate: DropListModel.
	self focusOrder
		add: input;
		add: dropList