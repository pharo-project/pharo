initialize-release
initializeWithLog: aLog

	self initialize.

	log := aLog.
	systemAnnouncer := SystemAnnouncer uniqueInstance.
	testAnnouncer := TestCase historyAnnouncer.
	jobAnnouncer := Job jobAnnouncer.
	
	entryReferenceByEvent := IdentityDictionary new.