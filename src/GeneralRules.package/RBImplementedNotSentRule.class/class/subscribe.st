announcement
subscribe
	<systemEventRegistration>
	self unsubscribe.
	
	SystemAnnouncer uniqueInstance weak
		when: MethodAdded    send: #methodAdded: to: self;
		when: MethodRemoved  send: #methodRemoved: to: self;
		when: MethodModified send: #methodModified: to: self