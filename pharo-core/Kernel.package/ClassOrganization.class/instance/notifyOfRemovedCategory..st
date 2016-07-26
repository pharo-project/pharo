notifications
notifyOfRemovedCategory: protocolName
	
	self hasSubject ifFalse: [ ^ self ].
	
	SystemAnnouncer uniqueInstance 
		protocolRemoved: protocolName 
		inClass: self subject