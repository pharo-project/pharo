initialization
initializeWidgets
	list := self instantiate: FastTableModel.
	addDeleteButtons := self
		instantiate: AddDeleteButtons.
	topUpDownBottomButtons := self
		instantiate: TopUpDownBottomButtons.
	self addButton help: 'Add a new item to the list'.
	self removeButton
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