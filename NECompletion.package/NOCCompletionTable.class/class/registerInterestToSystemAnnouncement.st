class initialization
registerInterestToSystemAnnouncement
	<systemEventRegistration>
	
	SystemAnnouncer uniqueInstance 
		unsubscribe: self.
		
	SystemAnnouncer uniqueInstance weak subscribe: MethodAdded, MethodModified 
			send: #methodChanged: 
			to: self.
		