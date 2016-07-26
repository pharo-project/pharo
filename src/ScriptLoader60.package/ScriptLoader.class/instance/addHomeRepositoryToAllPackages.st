mc related utils
addHomeRepositoryToAllPackages
	"self new addHomeRepositoryToAllPackages"
	
	self allManagers do: [:each | 
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