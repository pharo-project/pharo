initialization
initializeWidgets

	list := self instantiate: FastTableModel.
	addButton := self newButton.
	removeButton := self newButton.
	upButton := self newButton.
	downButton := self newButton.
	topButton := self newButton.
	bottomButton := self newButton.

	addButton
		icon: (Smalltalk ui icons iconNamed: #addIcon);
		help: 'Add a new item to the list'.
	
	removeButton 
		icon: (Smalltalk ui icons iconNamed: #deleteIcon);
		help: 'Remove a item from the list'.
		
	upButton
		icon: (Smalltalk ui icons iconNamed: #upIcon);
		help: 'Move this item up from one element'.
		
	downButton
		icon: (Smalltalk ui icons iconNamed: #downIcon);
		help: 'Move this item down from one element'.
		
	topButton
		icon: (Smalltalk ui icons iconNamed: #topIcon);
		help: 'Move this item on the first position of the list'.

	bottomButton
		icon: (Smalltalk ui icons iconNamed: #bottomIcon);
		help: 'Move this item on the last position of the list'.