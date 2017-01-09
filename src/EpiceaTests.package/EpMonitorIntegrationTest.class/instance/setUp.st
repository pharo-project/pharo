running
setUp
	super setUp.
	
	logBuilder := EpTestLogBuilder new.
	classFactory := ClassFactoryForTestCase new.
	monitor := EpMonitor newWithLog: logBuilder log.
