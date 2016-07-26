event registration
registerForNotifications
	"self registerForNotifications"
	<systemEventRegistration>
	
	MCPackageManager announcer: nil.
	SystemAnnouncer uniqueInstance unsubscribe: self.
	self registerInterestOnSystemChangesOnAnnouncer: SystemAnnouncer uniqueInstance.