searching
lookUpForRemovedDependentPackages: oldDependentPackages with: newDependentPackages from: aPackage

	| removed |
	removed := oldDependentPackages reject: [ :package | newDependentPackages includes: package ].
	removed do: [ :each | self addRemovedDependentPackage: each from: aPackage ].