mc related utils
addRepositoryTaskForcesToAllPackages
	"self new addRepositoryTaskForcesToAllPackages"
	
	self allManagers do: [:each | 
		each repositoryGroup
			 addRepository: self repositoryTaskForces
			].
	
	