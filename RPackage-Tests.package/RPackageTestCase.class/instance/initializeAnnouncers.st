announcer handling
initializeAnnouncers
	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.

	SystemAnnouncer announcer: (announcerForTest := SystemAnnouncer new).