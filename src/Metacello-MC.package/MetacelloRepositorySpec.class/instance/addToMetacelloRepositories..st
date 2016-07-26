adding
addToMetacelloRepositories: aMetacelloRepositoriesSpec

	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec addMember 
			name: self name;
			spec: self;
			yourself)