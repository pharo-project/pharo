running
setUp
	super setUp.
	identifier := GlobalIdentifier basicNew initialize.
	self setUpPersistenceStrategyAndChecker