cleaning
addHomeRepositoryToAllPackagesExcept: aCollection
	"self new addHomeRepositoryToAllPackages"
	
	(self allManagers \ aCollection) do: [:each | 
		each repositoryGroup
			addRepository: self repository;
			addRepository: self inboxRepository;
			addRepository: self repository50;
			addRepository: self inboxRepository50].
		
	MCRepositoryGroup default
		addRepository: self repository;
		addRepository: self inboxRepository;
		addRepository: self repository50;
		addRepository: self inboxRepository50