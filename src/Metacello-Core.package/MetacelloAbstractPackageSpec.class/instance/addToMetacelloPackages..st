adding
addToMetacelloPackages: aMetacelloPackagesSpec

	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec addMember 
			name: self name;
			spec: self;
			yourself)