*metacello-mc
addToMetacelloRepositories: aMetacelloRepositoriesSpec

	| spec |
	spec := 
		(aMetacelloRepositoriesSpec project repositorySpec)
			description: self;
			yourself.
	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec addMember 
			name: spec name;
			spec: spec;
			yourself)