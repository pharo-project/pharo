testing
shouldCallPreviousPersistence
	^ self preferences exists not and: [ self hasPreviousPersistence ]