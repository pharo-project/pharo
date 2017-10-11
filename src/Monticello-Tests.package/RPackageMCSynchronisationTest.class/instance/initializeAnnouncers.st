announcer handling
initializeAnnouncers
	super initializeAnnouncers.
	
	oldAnnouncer := MCWorkingCopy announcer.
	MCWorkingCopy announcer: announcerForTest.