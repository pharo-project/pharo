running
setUp 
	super setUp. 
	
	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer announcer: nil.
	
	factory := ClassFactoryForTestCase new.