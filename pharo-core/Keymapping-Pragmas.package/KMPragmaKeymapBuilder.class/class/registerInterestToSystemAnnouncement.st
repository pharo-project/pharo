system-events
registerInterestToSystemAnnouncement
	<systemEventRegistration>
	SystemAnnouncer uniqueInstance 
		unsubscribe: self.
	SystemAnnouncer uniqueInstance weak subscribe: MethodAdded, MethodModified, MethodRemoved 
		send: #event: 
		to: self.