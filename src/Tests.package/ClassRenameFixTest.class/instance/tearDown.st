running
tearDown

	self removeEverythingInSetFromSystem: testsChangeSet.
	ChangeSet newChanges: previousChangeSet.
	ChangeSet removeChangeSet: testsChangeSet.
	previousChangeSet := nil.
	testsChangeSet := nil.
	SystemAnnouncer uniqueInstance unsubscribe: self.
	super tearDown.