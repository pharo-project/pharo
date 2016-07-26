adding
addToMetacelloPackages: aMetacelloPackagesSpec

	| spec |
	spec := (aMetacelloPackagesSpec project projectReferenceSpec)
			name: self name;
			projectReference: self copy;
			yourself.
	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec addMember 
			name: spec name;
			spec: spec;
			yourself)