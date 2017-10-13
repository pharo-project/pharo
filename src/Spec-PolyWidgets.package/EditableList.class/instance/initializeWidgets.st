initialization
initializeWidgets
	list := self instantiate: FastTablePresenter.
	addButton := self newButton.
	removeButton := self newButton.
	upButton := self newButton.
	downButton := self newButton.
	topButton := self newButton.
	bottomButton := self newButton.
	addButton
		icon: (self iconNamed: #addIcon);
		help: 'Add a new item to the list'.
	removeButton
		icon: (self iconNamed: #deleteIcon);
		help: 'Remove a item from the list'.
	self upButton
		help: 'Move this item up from one element'.
	self downButton
		help: 'Move this item down from one element'.
	self topButton
		help: 'Move this item on the first position of the list'.
	self bottomButton
		help: 'Move this item on the last position of the list'.
	self focusOrder
		add: addDeleteButtons;
		add: topUpDownBottomButtons