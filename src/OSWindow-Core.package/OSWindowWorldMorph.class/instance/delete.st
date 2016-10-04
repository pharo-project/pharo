submorphs-add/remove
delete
	closeCallback value.
	self class removeExtraWorld: self.
	osWindow destroy.
	super delete