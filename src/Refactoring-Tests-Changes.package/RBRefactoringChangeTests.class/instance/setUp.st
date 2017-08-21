running
setUp
	super setUp.
	self createMockClass.
	changes := RBRefactoryChangeManager changeFactory compositeRefactoryChangeNamed: 'testing'