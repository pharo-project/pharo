brick-interactable-actions
doubleClick: anEvent

	self announce: #onDoubleClicked event: anEvent.
	^ false