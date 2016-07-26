initialization
initializeWithLog: aLog withViewClass: aViewClass

	andFilter := EpAndFilter withAll: #().
	log := aLog. 
	view := aViewClass newWithBrowser: self.

	commentsLog := EpMonitor current log.
	commitLog := EpMonitor current log.

	self initialize.

