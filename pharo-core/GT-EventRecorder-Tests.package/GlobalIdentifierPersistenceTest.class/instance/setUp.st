running
setUp
	super setUp.
	self setUpPreferences.
	persistence := self persistenceClass new preferences: preferences; yourself.