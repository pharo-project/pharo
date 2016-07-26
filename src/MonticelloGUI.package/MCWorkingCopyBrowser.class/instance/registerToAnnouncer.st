initialization
registerToAnnouncer
	SystemAnnouncer uniqueInstance weak
		when: MCPackageModified send: #update: to: self;
		when: MCWorkingCopyCreated send: #update: to: self;
		when: MCWorkingCopyDeleted send: #update: to: self;
		when: MCRepositoryAdded send: #updateRepositoryList to: self;
		when: MCRepositoryRemoved send: #updateRepositoryList to: self.