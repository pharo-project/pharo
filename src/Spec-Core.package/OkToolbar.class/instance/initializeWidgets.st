initialization
initializeWidgets
	
	
	okButton := self newButton.
	self registerOkButtonEvents.
	
	self focusOrder
		add: okButton