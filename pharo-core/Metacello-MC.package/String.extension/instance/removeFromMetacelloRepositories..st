*metacello-mc
removeFromMetacelloRepositories: aMetacelloRepositoriesSpec

	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec removeMember 
			name: self;
			yourself)