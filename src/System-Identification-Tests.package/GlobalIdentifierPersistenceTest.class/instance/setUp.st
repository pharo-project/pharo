running
setUp
	super setUp.
	self setUpPreferencesAndChecker.
	persistence := self persistenceClass new preferences: preferences; yourself.
	persistence checker: checker.