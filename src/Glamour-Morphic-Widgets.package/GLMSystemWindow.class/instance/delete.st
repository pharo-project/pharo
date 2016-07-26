open/close
delete
	self model notNil ifTrue: [self model unregisterFromAllAnnouncements].
	super delete