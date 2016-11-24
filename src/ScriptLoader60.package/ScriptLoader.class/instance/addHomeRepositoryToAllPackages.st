mc related utils
addHomeRepositoryToAllPackages
	"self new addHomeRepositoryToAllPackages"
	
	self allManagers do: [:each | 
		each repositoryGroup
			addRepository: self repository;
			addRepository: self inboxRepository].
		
	MCRepositoryGroup default
		addRepository: self repository;
		addRepository: self inboxRepository