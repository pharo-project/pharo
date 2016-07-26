brick-interactable-actions
press
	super press.
	
	self class asyncTaskUniqueInstance terminate.
	self class reset.