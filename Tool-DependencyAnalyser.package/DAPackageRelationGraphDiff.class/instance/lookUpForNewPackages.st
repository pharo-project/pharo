searching
lookUpForNewPackages
			
	| newPackages |
	newPackages := newRelationGraph seenPackages reject: [ :package | oldRelationGraph includesSeenPackage: package ].
	newPackages do: [ :each | self addNewPackage: each ].