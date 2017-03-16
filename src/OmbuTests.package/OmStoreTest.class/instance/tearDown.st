running
tearDown
	super tearDown.
	
	store flush.
	otherStore flush.
