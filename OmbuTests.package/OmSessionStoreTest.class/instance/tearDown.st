running
tearDown
	super tearDown.
	
	(self directoryForTests / 'test-sessions-main') ensureDeleteAll.
	(self directoryForTests / 'test-sessions-other') ensureDeleteAll.