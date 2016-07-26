removing
removeFromMetacelloPackages: aMetacelloPackagesSpec

	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec removeMember 
			name: self name;
			spec: self;
			yourself)