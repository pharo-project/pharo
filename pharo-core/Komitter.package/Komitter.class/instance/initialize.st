initialize
initialize

	super initialize.
	self registerToAnnouncements.
	stagingArea := nil asValueHolder.
	lastCommit := nil asValueHolder.