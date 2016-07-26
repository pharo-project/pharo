brick-interactable-actions
click: anEvent
	self announce: #onClicked event: anEvent.
	^ false