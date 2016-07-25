notifications
notifyOfAddedCategory: protocolName
	
	self hasSubject ifFalse: [ ^ self ].
	
	SystemAnnouncer uniqueInstance 
		protocolAdded: protocolName 
		inClass: self subject