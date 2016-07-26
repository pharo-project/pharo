running
setUp
	super setUp.
	
	monitor := EpMonitor newWithLog: self newLog.
	
	classFactory := ClassFactoryForTestCase new.