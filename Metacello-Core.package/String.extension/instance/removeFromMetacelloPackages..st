*Metacello-Core
removeFromMetacelloPackages: aMetacelloPackagesSpec

	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec removeMember 
			name: self;
			yourself)