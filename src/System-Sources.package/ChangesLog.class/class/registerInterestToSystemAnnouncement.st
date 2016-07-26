accessing
registerInterestToSystemAnnouncement
	<systemEventRegistration>
	
	DefaultInstance ifNotNil: [:defaultInstance |
		SystemAnnouncer uniqueInstance unsubscribe: defaultInstance.
		defaultInstance registerToAnnouncements. ].
