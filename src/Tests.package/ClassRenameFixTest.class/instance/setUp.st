running
setUp

	previousChangeSet := ChangeSet current.
	testsChangeSet := ChangeSet new.
	ChangeSet newChanges: testsChangeSet.
	SystemAnnouncer uniqueInstance weak
		when: ClassRenamed
		send: #verifyRenameEvent:
		to: self.
	super setUp