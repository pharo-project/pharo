protocol
delete
	
	super delete.
	self triggerEvent: #tabManagerDeleted with: self