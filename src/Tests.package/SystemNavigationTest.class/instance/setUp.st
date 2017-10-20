running
setUp
	super setUp. 
	
	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer announcer: nil.
	
	navigator := self createSystemNavigationToTest.
	classFactory := self createClassFactory.