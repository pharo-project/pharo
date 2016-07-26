private
mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec

	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec mergeMember 
			name: self name;
			spec: self;
			yourself)