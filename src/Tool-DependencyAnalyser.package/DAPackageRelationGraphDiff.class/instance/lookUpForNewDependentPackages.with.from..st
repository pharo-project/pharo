searching
lookUpForNewDependentPackages: newDependentPackages with: oldDependentPackages from: aPackage
	
	| newDependent |
	newDependent := newDependentPackages reject: [ :package | oldDependentPackages includes: package ].
	newDependent do: [ :each | self addNewDependentPackage: each from: aPackage ].