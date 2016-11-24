cleaning
addHomeRepositoryToAllPackagesExcept: aCollection
	"self new addHomeRepositoryToAllPackages"
	
	(self allManagers \ aCollection) do: [:each | 
		each repositoryGroup
			addRepository: self repository;
			addRepository: self inboxRepository ].
		
	MCRepositoryGroup default
		addRepository: self repository;
		addRepository: self inboxRepository