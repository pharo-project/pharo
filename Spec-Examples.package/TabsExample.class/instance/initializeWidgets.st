initialization
initializeWidgets

	manager := self newTabManager.
	manager whenTabSelected: [ self updateTitle ].
	self populateManager