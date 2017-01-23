initialization
subscribeToSnapshotDone

	SystemAnnouncer uniqueInstance weak
		when: SnapshotDone 
		send: #store "Force check whether the store needs reset"
		to: self.	