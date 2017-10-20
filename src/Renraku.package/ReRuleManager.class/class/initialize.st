class initialization
initialize
	self allInstances do: [ :inst |
		self flag: 'remove in future. this is just to stop
		            the announcements from choaking old instances'.
		SystemAnnouncer uniqueInstance unsubscribe: inst ].

	self reset.
	self subscribe.