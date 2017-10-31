resources
setUpOtherStore

	^ OmSessionStore newWithBaseLocator: (self directoryForTests / 'test-sessions-other') ensureCreateDirectory