running
setUp
	super setUp.
	
	logBuilder := EpTestLogBuilder new
		useLogWithSessionStore;
		yourself.
	classFactory := ClassFactoryForTestCase new.
	monitor := EpMonitor newWithLog: logBuilder log.
