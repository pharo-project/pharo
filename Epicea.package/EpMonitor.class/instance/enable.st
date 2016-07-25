enabling
enable
	"Enable monitoring IDE announcements."
	
	self isEnabled ifTrue: [ ^ self ].
	self subscribeToSystemAnnouncer.
	self subscribeToTestAnnouncer.
	self subscribeToJobAnnouncer.