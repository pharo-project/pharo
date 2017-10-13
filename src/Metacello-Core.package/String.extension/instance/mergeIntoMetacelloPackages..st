*metacello-core
mergeIntoMetacelloPackages: aMetacelloPackagesSpec

	| spec |
	spec := 
		(aMetacelloPackagesSpec project packageSpec)
			file: self;
			yourself.
	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec mergeMember 
			name: spec name;
			spec: spec;
			yourself)