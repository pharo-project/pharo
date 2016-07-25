initialization
initializeWidgets

	protocols := self newList.
	label := self newLabel.
	
	label label: 'Protocol'.
	
	self focusOrder add: protocols