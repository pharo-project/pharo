merging
mergeIntoMetacelloPackages: aMetacelloPackagesSpec

	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec mergeMember 
			name: self name;
			spec: self;
			yourself)