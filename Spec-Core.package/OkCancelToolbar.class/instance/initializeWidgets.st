initialization
initializeWidgets

	super initializeWidgets.
	cancelButton := self newButton.

	self registerCancelButtonEvents.
	self focusOrder
		add: cancelButton