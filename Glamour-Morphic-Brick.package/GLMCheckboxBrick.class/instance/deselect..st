brick-interactable-actions
deselect: anEvent
	super deselect: anEvent.
	self box deselect: anEvent.
	self label deselect: anEvent