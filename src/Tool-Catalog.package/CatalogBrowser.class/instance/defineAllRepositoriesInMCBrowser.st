actions
defineAllRepositoriesInMCBrowser
	"Define all the repositories as a repository browsable from within the MC Browser. This is is simpler to copy a configuration to the meta repositories."
	self availableRepositoryURLStrings do: [ :each | 
		MCRepositoryGroup default 
			addRepository: (self repositoryObjectFromURLString: each)] 