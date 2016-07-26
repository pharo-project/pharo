initialization
initializeWidgets

	list := self newList.
	inspectIt := self newButton.
	
	self setupMenu.
	
	list displayBlock: [ :assoc | assoc value asString].
	numItems := 7.
	self refreshItems.
	
	inspectIt label: 'Inspect ...'.
	inspectIt enabled: false.