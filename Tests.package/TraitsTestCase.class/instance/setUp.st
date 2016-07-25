running
setUp

	super setUp.
	oldAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer announcer: nil.
