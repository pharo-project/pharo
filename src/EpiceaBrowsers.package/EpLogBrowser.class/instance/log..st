initialization
log: aLog

	log ifNotNil: [ self unsubscribeFromLog ].
	log := aLog.
	self subscribeToLog.