announcement handling
subscribe

	self unsubscribe.
	
	SystemAnnouncer uniqueInstance weak
		when: ClassAdded   send: #classAdded: 	to: self;
		when: ClassRemoved send: #classRemoved: to: self.