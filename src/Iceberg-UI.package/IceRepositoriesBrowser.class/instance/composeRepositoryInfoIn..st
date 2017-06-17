building
composeRepositoryInfoIn: composite
	self addRepositoryInfoTo: composite.
	self composePackagesListIn: composite.
	self addRepositoryRemotesTo: composite.
	self addRepositoryBranchesTo: composite.
	