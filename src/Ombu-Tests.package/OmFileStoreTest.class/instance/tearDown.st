running
tearDown
	super tearDown.
	
	store fileReference ensureDelete.
	otherStore fileReference ensureDelete.