enabling
enable
	"Enable monitoring IDE announcements."
	
	self isEnabled ifFalse: [ 			
		self subscribeToSystemAnnouncer.
		self subscribeToTestAnnouncer.
		self subscribeToJobAnnouncer. ].

	self announceMonitorStateUpdated.
