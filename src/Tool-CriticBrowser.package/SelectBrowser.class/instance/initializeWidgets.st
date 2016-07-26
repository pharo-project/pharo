initialization
initializeWidgets

	addButtonModel := self newButton.
	addAllButtonModel := self newButton. 
	removeButtonModel := self newButton.
	removeAllButtonModel := self newButton.
	nextButtonModel := self newButton.
		
	self setFocusOrder.

	self setAddButtonModel. 
	self setAddAllButtonModel. 
	self setRemoveButtonModel. 
	self setRemoveAllButtonModel. 
	self setNextButtonModel. 