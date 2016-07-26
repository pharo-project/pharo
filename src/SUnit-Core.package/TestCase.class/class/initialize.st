initialize - event
initialize
	super initialize.
	SystemAnnouncer uniqueInstance unsubscribe: self.
	SystemAnnouncer uniqueInstance weak
		when: MethodAdded, MethodModified, MethodRemoved 
		send: #methodChanged: 
		to: self.