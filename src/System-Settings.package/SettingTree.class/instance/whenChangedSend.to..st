updating
whenChangedSend: aSelector to: aTarget
	self collector whenChangedSend: aSelector to: aTarget.
	self collector whenResetSend: aSelector to: aTarget.