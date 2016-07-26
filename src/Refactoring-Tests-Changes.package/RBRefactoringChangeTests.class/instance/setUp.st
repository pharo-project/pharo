running
setUp
	super setUp.
	self createMockClass.
	changes := RBCompositeRefactoryChange named: 'testing'