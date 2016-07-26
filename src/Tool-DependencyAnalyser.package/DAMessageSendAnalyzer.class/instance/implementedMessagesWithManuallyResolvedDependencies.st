computing
implementedMessagesWithManuallyResolvedDependencies
	^ (self manuallyResolvedDependencies 
		flatCollect: [ :name | (RPackageOrganizer default packageNamed: name) selectors ]
		as: Set)
			addAll: self implementedMessages;
			yourself