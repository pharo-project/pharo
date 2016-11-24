private
addRepositoryDefaultToAllPackages
	"self new addRepositoryDefaultToAllPackages"
	
	self allManagers do: [:each | 
		each repositoryGroup
			 addRepository: self repository
			].
	
	