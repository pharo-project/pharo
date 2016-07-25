running
tearDown 
	super tearDown.
	
	SystemAnnouncer uniqueInstance unsubscribe: collectedAnnouncements.