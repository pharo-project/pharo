resources
setUpStore	

	^ OmSessionStore newWithBaseLocator: (self directoryForTests / 'test-sessions-main') ensureCreateDirectory