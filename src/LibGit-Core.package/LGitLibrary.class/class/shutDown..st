system startup
shutDown: quitting
	quitting ifFalse: [ ^ self ].
	self isInitialized ifFalse: [ ^self ].
	self uniqueInstance shutdownLibGit2.