running
tearDown
	super tearDown.
	
	self tearDownOneTriggerWithFourChildrenLog.
	self tearDownThreeEntriesWithAbsentPrior.
	self tearDownOneTriggerWithOneChildLog.