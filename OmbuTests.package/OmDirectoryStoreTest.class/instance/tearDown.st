running
tearDown
	super tearDown.
	
	store fileReference ensureDeleteAll.
	otherStore fileReference ensureDeleteAll.