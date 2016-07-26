mc related utils
removeAllRepositories
	"self new removeAllRepositories"
	
	self allManagers do: [:each |  each  repositoryGroup initialize].
	MCRepositoryGroup default initialize.
	