initialization
initializeWithLog: aLog

	self initialize.
	
	lastSessionFileStore := aLog store lowLevelFileStoreIfNone: [ nil ]. "Never nil?"
	hasLostChanges :=
		lastSessionFileStore
			ifNil: [ false ]
			ifNotNil: [ lastSessionFileStore isOutdated ].
