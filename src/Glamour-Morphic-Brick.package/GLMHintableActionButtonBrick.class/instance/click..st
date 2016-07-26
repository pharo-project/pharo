brick-interactable-actions
click: evt
	|result|
	result := super click: evt.
	self class asyncTaskUniqueInstance terminate.
	self class reset.
	^ result