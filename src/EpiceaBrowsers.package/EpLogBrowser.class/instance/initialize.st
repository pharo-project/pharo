initialization
initialize

	super initialize.
	
	andFilter := EpAndFilter withAll: #().
	commentsLog := EpMonitor current log.
	commitLog := EpMonitor current log.