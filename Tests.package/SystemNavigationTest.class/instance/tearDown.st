setUp-tearDown
tearDown
	super tearDown.
	self classFactory cleanUp. 
	SystemAnnouncer announcer: oldSystemAnnouncer.
	