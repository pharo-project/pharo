running
tearDown
	super tearDown.

	SystemAnnouncer announcer: oldSystemAnnouncer.
	
	factory cleanUp.